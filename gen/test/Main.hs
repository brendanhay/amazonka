module Main (main) where

import qualified Control.Comonad as Comonad
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Gen.AST (solve)
import Gen.AST.Subst (emptyInfo, emptyRef)
import Gen.Prelude
import Gen.Types
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

main :: IO ()
main = do
  cfg <- either assertFailure pure (Aeson.eitherDecode "{\"libraryName\":\"test\"}")
  defaultMain $
    testGroup
      "solve"
      [ testGroup "sensitiveViaLoopBreakerDropsRead" $
          orders fsxPairs <&> \(name, trees) ->
            testCase name $ rootsDerive cfg trees ["AA", "FS"] noRead,
        testGroup "sensitiveCycleOuterDropsRead" $
          orders [emrPair] <&> \(name, trees) ->
            testCase name $
              rootsDerive cfg trees ["Configuration", "ConfigurationOverrides"] noRead,
        testGroup "memberTypesMatchDeclarations" $
          orders (emrPair : fsxPairs) <&> \(name, trees) ->
            testCase name $ memberMismatches (solve cfg trees) @?= [],
        testCase "plainCycleKeepsRead" $ rootsDerive cfg [nodeTree] ["Node"] full
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

nodeTree :: Shape Prefixed
nodeTree = struct "Node" [("Children", list "NodeList" (ptr "Node"))]

rootsDerive :: Config -> [Shape Prefixed] -> [Text] -> [Derive] -> Assertion
rootsDerive cfg trees names expected =
  [(n, annDerivings <$> root (mkId n) out) | n <- names]
    @?= [(n, Just expected) | n <- names]
  where
    out = solve cfg trees

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

-- | A loop-breaker for the shape named @n@.
ptr :: Text -> Shape Prefixed
ptr n = ann n :< Ptr emptyInfo (TType (typeId (mkId n)) derivingBase)

refTo :: Shape Prefixed -> RefF (Shape Prefixed)
refTo s = emptyRef (identifier s) & refAnn .~ s
