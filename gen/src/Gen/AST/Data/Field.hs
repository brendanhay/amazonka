{-# LANGUAGE TemplateHaskell #-}

module Gen.AST.Data.Field where

import qualified Control.Comonad.Cofree as Cofree
import qualified Control.Lens as Lens
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import Gen.Prelude
import Gen.Text
import Gen.Types
import Language.Haskell.Exts.Syntax (Name (..))

-- | Convenience type to package up some information from the struct with the
-- related field, namely the memberId and the (Set.member required).
data Field = Field
  { _fieldMeta :: Metadata Identity,
    _fieldOrdinal :: Int,
    -- | The memberId from the struct members map.
    _fieldId :: Id,
    -- | The original struct member reference.
    _fieldRef :: Ref,
    -- | Does the struct have this member in the required set.
    _fieldRequire :: Bool,
    -- | Does the struct have this memeber marked as the payload.
    _fieldPayload :: Bool,
    _fieldPrefix :: Maybe Text,
    _fieldNamespace :: Maybe Text,
    _fieldDirection :: Maybe Direction
  }
  deriving (Show)

$(Lens.makeLenses ''Field)

instance IsStreaming Field where
  isStreaming = isStreaming . _fieldRef

instance TypeOf Field where
  typeOf f
    | isStreaming ref = typ
    | isKinded, isHeader = typ
    | f ^. fieldRequire = typ
    | otherwise = TMaybe typ
    where
      isKinded =
        case typ of
          TMap {} -> True
          TList {} -> True
          _ -> False

      isHeader =
        fieldLocation f
          `elem` map
            Just
            [ Headers,
              Header
            ]

      ref = f ^. fieldRef
      typ = fmap unBase64 (typeOf ref)

      unBase64 = \case
        Base64 | f ^. fieldPayload -> Bytes
        lit -> lit

instance HasInfo Field where
  info = fieldAnn . info

-- FIXME: Can just add the metadata to field as well since
-- the protocol/timestamp are passed in everywhere in the .Syntax module.
mkFields ::
  HasMetadata a Identity =>
  a ->
  Solved ->
  StructF (Shape Solved) ->
  [Field]
mkFields (Lens.view metadata -> m) s st =
  sortFields rs $
    zipWith mk [1 ..] $ HashMap.toList (st ^. members)
  where
    mk :: Int -> (Id, Ref) -> Field
    mk i (k, v) =
      Field
        { _fieldMeta = m,
          _fieldOrdinal = i,
          _fieldId = k,
          _fieldRef = v,
          _fieldRequire = req,
          _fieldPayload = pay,
          _fieldPrefix = p,
          _fieldNamespace = ns,
          _fieldDirection = d
        }
      where
        req = k `elem` rs
        pay = Just k == st ^. payload

        ns =
          (Lens.view xmlUri <$> v ^. refXMLNamespace)
            <|> (m ^. xmlNamespace)

    rs = st ^.. getRequired
    p = s ^. annPrefix

    d = case s ^. relMode of
      Uni x -> Just x
      Bi -> Nothing

-- | Ensures that isStreaming fields appear last in the parameter ordering,
-- but doesn't affect the rest of the order which is determined by parsing
-- of the JSON service definition.
sortFields :: [Id] -> [Field] -> [Field]
sortFields xs =
  zipWith (Lens.set fieldOrdinal) [1 ..]
    -- FIXME: optimise
    . List.sortBy (Ord.comparing isStreaming)
    . List.sortBy (Ord.comparing idx)
  where
    idx x = fromMaybe (-1) (List.elemIndex (_fieldId x) xs)

fieldAnn :: Lens' Field (Shape Solved)
fieldAnn = fieldRef . refAnn

fieldLens, fieldAccessor :: Field -> Text
fieldLens f = lensId (_fieldPrefix f) (_fieldId f)
fieldAccessor f = accessorId (_fieldId f)

fieldIsParam :: Field -> Bool
fieldIsParam f = not (fieldMaybe f) && not (fieldMonoid f)

fieldParamName :: Field -> Name ()
fieldParamName =
  Ident ()
    . Text.unpack
    . Text.cons 'p'
    . flip Text.snoc '_'
    . upperHead
    . typeId
    . _fieldId

fieldHelp :: Field -> Help
fieldHelp f =
  fromMaybe def (f ^. fieldRef . refDocumentation) <> ann (typeOf f)
  where
    ann (TMaybe t) = ann t
    ann (TSensitive t) = ann t
    ann (TLit Base64) = base64
    ann _ = mempty

    base64 =
      "--\n-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.\n\
      \-- The underlying isomorphism will encode to Base64 representation during\n\
      \-- serialisation, and decode from Base64 representation during deserialisation.\n\
      \-- This 'Lens' accepts and returns only raw unencoded data."

    def = "Undocumented member."

fieldLocation :: Field -> Maybe Location
fieldLocation = Lens.view (fieldRef . refLocation)

-- | Is the field reference or its parent annotation streaming?
fieldStream :: Field -> Bool
fieldStream x =
  x ^. fieldRef . refStreaming
    || x ^. fieldAnn . infoStreaming

-- | Does the field have its location set to 'Body'?
fieldBody :: Field -> Bool
fieldBody x =
  case fieldLocation x of
    Just Body -> True
    Nothing -> True
    _ -> fieldStream x

-- | Is this primitive field set as the payload in the parent shape?
fieldLitPayload :: Field -> Bool
fieldLitPayload x = _fieldPayload x && (fieldLit x || fieldBytes x)

-- This is hilariously brittle.
fieldBytes :: Field -> Bool
fieldBytes = typeMember (Right Bytes) . typeOf

fieldMaybe :: Field -> Bool
fieldMaybe x =
  case typeOf x of
    TMaybe {} -> True
    _ -> False

fieldMonoid :: Field -> Bool
fieldMonoid = elem DMonoid . derivingOf . Lens.view fieldAnn

fieldList1, fieldList, fieldMap, fieldLit :: Field -> Bool
fieldList1 f = fieldList f && nonEmpty f
fieldList = not . Lens.isn't _List . Cofree.unwrap . Lens.view fieldAnn
fieldMap = not . Lens.isn't _Map . Cofree.unwrap . Lens.view fieldAnn
fieldLit = not . Lens.isn't _Lit . Cofree.unwrap . Lens.view fieldAnn
