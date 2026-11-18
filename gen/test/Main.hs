module Main (main) where

import qualified Control.Comonad as Comonad
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Gen.AST.Cofree (attach, elaborate)
import Gen.AST.Prefix (prefixes)
import Gen.AST.Solve (solve)
import Gen.AST.Subst (emptyInfo, emptyRef)
import Gen.Prelude
import Gen.Types
import Test.Tasty (defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

main :: IO ()
main =
  defaultMain . localOption (mkTimeout 10000000) $
    testGroup
      "solve"
      [ testGroup "sensitiveViaLoopBreakerDropsRead" $
          orders fsxPairs <&> \(name, trees) ->
            testCase name $ rootsDerive trees ["AA", "FS"] noRead,
        testGroup "sensitiveCycleOuterDropsRead" $
          orders [emrPair] <&> \(name, trees) ->
            testCase name $
              rootsDerive trees ["Configuration", "ConfigurationOverrides"] noRead,
        testGroup "memberTypesMatchDeclarations" $
          orders fsxPairs <&> \(name, trees) ->
            testCase name $ solveAll trees >>= \out -> memberMismatches out @?= [],
        testCase "plainCycleKeepsRead" $ rootsDerive [nodeTree] ["Node"] full,
        testCase "pipelineFsxDropsRead" $
          pipelineDerives mempty fsxShapes ["AA", "FS", "Root"] noRead,
        testCase "pipelineEmrContainersDropsRead" $
          pipelineDerives mempty emrShapes ["Configuration", "ConfigurationOverrides"] noRead,
        testCase "replacementSeedWins" $
          pipelineDerives tagSeed tagShapes ["Parent", "Tag"] noRead,
        testCase "sensitiveLoopBreakerWrapsOnce" $ do
          out <- solveAll [filterTree]
          [typeOf x | x <- concatMap subtrees out, identifier x == mkId "Filter"]
            @?= replicate 2 (TSensitive (TType "Filter" noRead)),
        testGroup "occurrencesAgree" $
          [ testCase name $ solveAll trees >>= \out -> occurrenceMismatches out @?= []
          | (name, trees) <- ("filterTree", [filterTree]) : orders (emrPair : fsxPairs)
          ]
            ++ [ testCase name $
                   pipeline seed json >>= \out ->
                     occurrenceMismatches (HashMap.elems out) @?= []
               | (name, seed, json) <-
                   [ ("fsxShapes", mempty, fsxShapes),
                     ("emrShapes", mempty, emrShapes),
                     ("tagShapes", tagSeed, tagShapes)
                   ]
               ],
        testCase "replacedKeepsOwnType" $
          rootsDeriveWith extSeed [holdA, holdB] [("HoldA", noRead), ("HoldB", full)],
        testGroup "prefixIgnoresLoopBreakerOrder" $
          [("Alpha into Beta", "Alpha", "Beta"), ("Beta into Alpha", "Beta", "Alpha")] <&> \(name, outer, inner) ->
            testCase name $ do
              out <- either assertFailure pure (prefixes (referringInto outer inner))
              HashMap.map (isJust . _annPrefix . Comonad.extract) out
                @?= HashMap.fromList [(mkId outer, True), (mkId inner, True)],
        testCase "danglingPtrFails" $
          case solve mempty [struct "A" [("B", ptr "Ghost")]] of
            Left e -> assertBool e ("Ghost" `List.isInfixOf` e)
            Right _ -> assertFailure "solved a loop-breaker with no target",
        testCase "structlessCycleFails" $
          assertBool "solved a cycle with no struct on it" $
            isLeft (solve mempty [list "Loop" (ptr "Loop")])
      ]

noRead, full :: [Derive]
noRead = [DEq, DShow, DGeneric, DHashable, DNFData]
full = [DEq, DRead, DShow, DGeneric, DHashable, DNFData]

type Named a = (String, a)

-- | Each pair of trees in both list orders, since 'solve' visits its input
-- in order.
orders :: [(Named a, Named a)] -> [Named [a]]
orders pairs =
  concat
    [ [(x ++ " " ++ y, [a, b]), (y ++ " " ++ x, [b, a])]
    | ((x, a), (y, b)) <- pairs
    ]

-- | The trees 'elaborate' produces for a struct @FS@ with a sensitive member
-- and a list of @AA@, where @AA@ refers back to @FS@. The shape elaborated
-- second is the one whose root holds the loop-breaker.
fsxPairs :: [(Named (Shape Prefixed), Named (Shape Prefixed))]
fsxPairs =
  [ (("fsTree", fsTree), ("aaPtr", aaPtr)),
    (("aaTree", aaTree), ("fsPtr", fsPtr))
  ]
  where
    fsTree = fs (struct "AA" [("Target", ptr "FS"), name])
    aaPtr = struct "AA" [("Target", ptr "FS"), name]

    aaTree = struct "AA" [("Target", fs (ptr "AA")), name]
    fsPtr = fs (ptr "AA")

    fs item = struct "FS" [("Pw", sensitive (text "Pw")), ("Actions", list "AAList" item)]
    name = ("Name", text "Str")

-- | The trees 'elaborate' produces when it visits @Configuration@ before
-- @ConfigurationOverrides@, which only refers into the cycle.
emrPair :: (Named (Shape Prefixed), Named (Shape Prefixed))
emrPair = (("cfgTree", cfgTree), ("ovTree", ovTree))
  where
    cfgTree =
      struct
        "Configuration"
        [ ("Classification", text "Str"),
          ("Secret", sensitive (text "Secret")),
          ("Configurations", configurations)
        ]
    ovTree = struct "ConfigurationOverrides" [("ApplicationConfiguration", configurations)]

    configurations = list "ConfigurationList" (ptr "Configuration")

-- | Two structs holding replacements that share a name and differ in what
-- they can derive.
holdA, holdB :: Shape Prefixed
holdA = struct "HoldA" [("Value", replaced "Ext" (TType "Ext" noRead))]
holdB = struct "HoldB" [("Value", replaced "Ext" (TType "Ext" full))]

extSeed :: HashMap Id TType
extSeed = HashMap.fromList [(mkId "Ext", TType "Ext" noRead)]

-- | A recursive struct @inner@ and a struct @outer@ that only refers into the
-- cycle, as 'elaborate' produces them when it visits @inner@ first. Which
-- root 'prefixes' visits first depends on how the two names hash, so callers
-- pass each pair of names both ways round to cover both orders.
referringInto :: Text -> Text -> HashMap Id (Shape Related)
referringInto outer inner =
  HashMap.fromList
    [ (identifier t, _annRelated <$> t)
    | t <-
        [ struct outer [("Items", items)],
          struct inner [("Name", text "Str"), ("Items", items)]
        ]
    ]
  where
    items = list "ItemList" (ptr inner)

nodeTree :: Shape Prefixed
nodeTree = struct "Node" [("Children", list "NodeList" (ptr "Node"))]

-- | A sensitive struct that refers to itself. 'elaborate' copies the target's
-- 'Info' onto the loop-breaker, so the loop-breaker is sensitive too.
filterTree :: Shape Prefixed
filterTree =
  sensitive $
    struct
      "Filter"
      [ ("AndAll", list "FilterList" (sensitive (ptr "Filter"))),
        ("Key", text "Str")
      ]

fsxShapes :: Aeson.Value
fsxShapes =
  Aeson.object
    [ "Root" .= structure [("Fs", "FS")],
      "FS" .= structure [("Pw", "Pw"), ("Actions", "AAList")],
      "Pw" .= Aeson.object ["type" .= string, "sensitive" .= True],
      "AAList" .= listOf "AA",
      "AA" .= structure [("Target", "FS"), ("Name", "Str")],
      "Str" .= Aeson.object ["type" .= string]
    ]

emrShapes :: Aeson.Value
emrShapes =
  Aeson.object
    [ "Configuration"
        .= structure
          [ ("Classification", "Str"),
            ("Properties", "SensitiveMap"),
            ("Configurations", "ConfigurationList")
          ],
      "SensitiveMap"
        .= Aeson.object
          [ "type" .= ("map" :: Text),
            "sensitive" .= True,
            "key" .= shapeRef "Str",
            "value" .= shapeRef "Str"
          ],
      "ConfigurationList" .= listOf "Configuration",
      "ConfigurationOverrides" .= structure [("ApplicationConfiguration", "ConfigurationList")],
      "Str" .= Aeson.object ["type" .= string]
    ]

-- | A struct whose name is also the name of an override's replacement type.
tagShapes :: Aeson.Value
tagShapes =
  Aeson.object
    [ "Parent" .= structure [("Tag", "Tag")],
      "Tag" .= structure [("Key", "Str")],
      "Str" .= Aeson.object ["type" .= string]
    ]

tagSeed :: HashMap Id TType
tagSeed = HashMap.fromList [(mkId "Tag", TType "Tag" noRead)]

structure :: [(Aeson.Key, Text)] -> Aeson.Value
structure ms =
  Aeson.object
    [ "type" .= ("structure" :: Text),
      "members" .= Aeson.object [k .= shapeRef v | (k, v) <- ms]
    ]

listOf :: Text -> Aeson.Value
listOf item = Aeson.object ["type" .= ("list" :: Text), "member" .= shapeRef item]

shapeRef :: Text -> Aeson.Value
shapeRef n = Aeson.object ["shape" .= n]

string :: Text
string = "string"

-- | Botocore shape definitions run through the generator's stages up to and
-- including 'solve'.
pipeline :: HashMap Id TType -> Aeson.Value -> IO (HashMap Id (Shape Solved))
pipeline seed json = either assertFailure pure $ do
  raw <- Aeson.Types.parseEither Aeson.parseJSON json :: Either String (HashMap Id (ShapeF ()))
  trees <- elaborate raw
  prefixes (attach Related mempty <$> trees) >>= solve seed

pipelineDerives :: HashMap Id TType -> Aeson.Value -> [Text] -> [Derive] -> Assertion
pipelineDerives seed json names expected = do
  out <- pipeline seed json
  [(n, annDerivings <$> HashMap.lookup (mkId n) out) | n <- names]
    @?= [(n, Just expected) | n <- names]

solveAll :: [Shape Prefixed] -> IO [Shape Solved]
solveAll = either assertFailure pure . solve mempty

rootsDerive :: [Shape Prefixed] -> [Text] -> [Derive] -> Assertion
rootsDerive trees names expected =
  rootsDeriveWith mempty trees [(n, expected) | n <- names]

rootsDeriveWith :: HashMap Id TType -> [Shape Prefixed] -> [(Text, [Derive])] -> Assertion
rootsDeriveWith seed trees expected = do
  out <- either assertFailure pure (solve seed trees)
  [(n, annDerivings <$> root (mkId n) out) | (n, _) <- expected]
    @?= [(n, Just ds) | (n, ds) <- expected]

-- | Struct members whose derivings differ from those of the declaration they
-- refer to, as @(struct, member, member derivings, declaration derivings)@.
-- Members referring to a shape with no root in the input are skipped.
memberMismatches :: [Shape Solved] -> [(Text, Text, [Derive], [Derive])]
memberMismatches out =
  [ (memberId (identifier a), memberId k, derivingOf r, expected)
  | tree <- out,
    a :< Struct st <- subtrees tree,
    (k, r) <- HashMap.toList (st ^. members),
    not (isStreaming r),
    Just expected <- [annDerivings <$> root (identifier (r ^. refAnn)) out],
    derivingOf r /= expected
  ]

-- | Nodes whose type differs from that of the root with the same 'Id', as
-- @(shape, node type, root type)@.
occurrenceMismatches :: [Shape Solved] -> [(Text, TType, TType)]
occurrenceMismatches out =
  [ (memberId (identifier x), typeOf x, typeOf r)
  | tree <- out,
    x <- subtrees tree,
    Just r <- [root (identifier x) out],
    typeOf x /= typeOf r
  ]

subtrees :: Shape a -> [Shape a]
subtrees = foldr (:) [] . Comonad.duplicate

root :: (HasId a) => Id -> [Shape a] -> Maybe (Shape a)
root n = List.find ((n ==) . identifier)

-- | The derivings of a node's 'Solved' annotation, which is what its
-- declaration is rendered from.
annDerivings :: Shape Solved -> [Derive]
annDerivings = derivingOf . Comonad.extract

ann :: Text -> Prefixed
ann n = Prefixed (Related (mkId n) mempty) Nothing

struct :: Text -> [(Text, Shape Prefixed)] -> Shape Prefixed
struct n fields =
  ann n :< Struct (StructF emptyInfo (HashMap.fromList (map member fields)) [] Nothing)
  where
    member (k, s) = (mkId k, refTo s)

list :: Text -> Shape Prefixed -> Shape Prefixed
list n item = ann n :< List (ListF emptyInfo (refTo item))

text :: Text -> Shape Prefixed
text n = ann n :< Lit emptyInfo Text

sensitive :: Shape Prefixed -> Shape Prefixed
sensitive = info . infoSensitive .~ True

-- | An override's replacement type standing in for the shape named @n@.
replaced :: Text -> TType -> Shape Prefixed
replaced n t = ann n :< Replaced emptyInfo t

-- | A loop-breaker for the shape named @n@.
ptr :: Text -> Shape Prefixed
ptr n = ann n :< Ptr emptyInfo

refTo :: Shape Prefixed -> RefF (Shape Prefixed)
refTo s = emptyRef (identifier s) & refAnn .~ s
