{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.Product where

import Network.AWS.CloudDirectory.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A unique identifier for an attribute.
--
--
--
-- /See:/ 'attributeKey' smart constructor.
data AttributeKey = AttributeKey'
  { _akSchemaARN :: !Text
  , _akFacetName :: !Text
  , _akName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akSchemaARN' - The Amazon Resource Name (ARN) of the schema that contains the facet and attribute.
--
-- * 'akFacetName' - The name of the facet that the attribute exists within.
--
-- * 'akName' - The name of the attribute.
attributeKey
    :: Text -- ^ 'akSchemaARN'
    -> Text -- ^ 'akFacetName'
    -> Text -- ^ 'akName'
    -> AttributeKey
attributeKey pSchemaARN_ pFacetName_ pName_ =
  AttributeKey'
    {_akSchemaARN = pSchemaARN_, _akFacetName = pFacetName_, _akName = pName_}


-- | The Amazon Resource Name (ARN) of the schema that contains the facet and attribute.
akSchemaARN :: Lens' AttributeKey Text
akSchemaARN = lens _akSchemaARN (\ s a -> s{_akSchemaARN = a})

-- | The name of the facet that the attribute exists within.
akFacetName :: Lens' AttributeKey Text
akFacetName = lens _akFacetName (\ s a -> s{_akFacetName = a})

-- | The name of the attribute.
akName :: Lens' AttributeKey Text
akName = lens _akName (\ s a -> s{_akName = a})

instance FromJSON AttributeKey where
        parseJSON
          = withObject "AttributeKey"
              (\ x ->
                 AttributeKey' <$>
                   (x .: "SchemaArn") <*> (x .: "FacetName") <*>
                     (x .: "Name"))

instance Hashable AttributeKey where

instance NFData AttributeKey where

instance ToJSON AttributeKey where
        toJSON AttributeKey'{..}
          = object
              (catMaybes
                 [Just ("SchemaArn" .= _akSchemaARN),
                  Just ("FacetName" .= _akFacetName),
                  Just ("Name" .= _akName)])

-- | The combination of an attribute key and an attribute value.
--
--
--
-- /See:/ 'attributeKeyAndValue' smart constructor.
data AttributeKeyAndValue = AttributeKeyAndValue'
  { _akavKey   :: !AttributeKey
  , _akavValue :: !TypedAttributeValue
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeKeyAndValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akavKey' - The key of the attribute.
--
-- * 'akavValue' - The value of the attribute.
attributeKeyAndValue
    :: AttributeKey -- ^ 'akavKey'
    -> TypedAttributeValue -- ^ 'akavValue'
    -> AttributeKeyAndValue
attributeKeyAndValue pKey_ pValue_ =
  AttributeKeyAndValue' {_akavKey = pKey_, _akavValue = pValue_}


-- | The key of the attribute.
akavKey :: Lens' AttributeKeyAndValue AttributeKey
akavKey = lens _akavKey (\ s a -> s{_akavKey = a})

-- | The value of the attribute.
akavValue :: Lens' AttributeKeyAndValue TypedAttributeValue
akavValue = lens _akavValue (\ s a -> s{_akavValue = a})

instance FromJSON AttributeKeyAndValue where
        parseJSON
          = withObject "AttributeKeyAndValue"
              (\ x ->
                 AttributeKeyAndValue' <$>
                   (x .: "Key") <*> (x .: "Value"))

instance Hashable AttributeKeyAndValue where

instance NFData AttributeKeyAndValue where

instance ToJSON AttributeKeyAndValue where
        toJSON AttributeKeyAndValue'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _akavKey),
                  Just ("Value" .= _akavValue)])

-- | Identifies the attribute name and value for a typed link.
--
--
--
-- /See:/ 'attributeNameAndValue' smart constructor.
data AttributeNameAndValue = AttributeNameAndValue'
  { _anavAttributeName :: !Text
  , _anavValue         :: !TypedAttributeValue
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttributeNameAndValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'anavAttributeName' - The attribute name of the typed link.
--
-- * 'anavValue' - The value for the typed link.
attributeNameAndValue
    :: Text -- ^ 'anavAttributeName'
    -> TypedAttributeValue -- ^ 'anavValue'
    -> AttributeNameAndValue
attributeNameAndValue pAttributeName_ pValue_ =
  AttributeNameAndValue'
    {_anavAttributeName = pAttributeName_, _anavValue = pValue_}


-- | The attribute name of the typed link.
anavAttributeName :: Lens' AttributeNameAndValue Text
anavAttributeName = lens _anavAttributeName (\ s a -> s{_anavAttributeName = a})

-- | The value for the typed link.
anavValue :: Lens' AttributeNameAndValue TypedAttributeValue
anavValue = lens _anavValue (\ s a -> s{_anavValue = a})

instance FromJSON AttributeNameAndValue where
        parseJSON
          = withObject "AttributeNameAndValue"
              (\ x ->
                 AttributeNameAndValue' <$>
                   (x .: "AttributeName") <*> (x .: "Value"))

instance Hashable AttributeNameAndValue where

instance NFData AttributeNameAndValue where

instance ToJSON AttributeNameAndValue where
        toJSON AttributeNameAndValue'{..}
          = object
              (catMaybes
                 [Just ("AttributeName" .= _anavAttributeName),
                  Just ("Value" .= _anavValue)])

-- | Represents the output of a batch add facet to object operation.
--
--
--
-- /See:/ 'batchAddFacetToObject' smart constructor.
data BatchAddFacetToObject = BatchAddFacetToObject'
  { _baftoSchemaFacet         :: !SchemaFacet
  , _baftoObjectAttributeList :: ![AttributeKeyAndValue]
  , _baftoObjectReference     :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAddFacetToObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baftoSchemaFacet' - Represents the facet being added to the object.
--
-- * 'baftoObjectAttributeList' - The attributes to set on the object.
--
-- * 'baftoObjectReference' - A reference to the object being mutated.
batchAddFacetToObject
    :: SchemaFacet -- ^ 'baftoSchemaFacet'
    -> ObjectReference -- ^ 'baftoObjectReference'
    -> BatchAddFacetToObject
batchAddFacetToObject pSchemaFacet_ pObjectReference_ =
  BatchAddFacetToObject'
    { _baftoSchemaFacet = pSchemaFacet_
    , _baftoObjectAttributeList = mempty
    , _baftoObjectReference = pObjectReference_
    }


-- | Represents the facet being added to the object.
baftoSchemaFacet :: Lens' BatchAddFacetToObject SchemaFacet
baftoSchemaFacet = lens _baftoSchemaFacet (\ s a -> s{_baftoSchemaFacet = a})

-- | The attributes to set on the object.
baftoObjectAttributeList :: Lens' BatchAddFacetToObject [AttributeKeyAndValue]
baftoObjectAttributeList = lens _baftoObjectAttributeList (\ s a -> s{_baftoObjectAttributeList = a}) . _Coerce

-- | A reference to the object being mutated.
baftoObjectReference :: Lens' BatchAddFacetToObject ObjectReference
baftoObjectReference = lens _baftoObjectReference (\ s a -> s{_baftoObjectReference = a})

instance Hashable BatchAddFacetToObject where

instance NFData BatchAddFacetToObject where

instance ToJSON BatchAddFacetToObject where
        toJSON BatchAddFacetToObject'{..}
          = object
              (catMaybes
                 [Just ("SchemaFacet" .= _baftoSchemaFacet),
                  Just
                    ("ObjectAttributeList" .= _baftoObjectAttributeList),
                  Just ("ObjectReference" .= _baftoObjectReference)])

-- | The result of a batch add facet to object operation.
--
--
--
-- /See:/ 'batchAddFacetToObjectResponse' smart constructor.
data BatchAddFacetToObjectResponse =
  BatchAddFacetToObjectResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAddFacetToObjectResponse' with the minimum fields required to make a request.
--
batchAddFacetToObjectResponse
    :: BatchAddFacetToObjectResponse
batchAddFacetToObjectResponse = BatchAddFacetToObjectResponse'


instance FromJSON BatchAddFacetToObjectResponse where
        parseJSON
          = withObject "BatchAddFacetToObjectResponse"
              (\ x -> pure BatchAddFacetToObjectResponse')

instance Hashable BatchAddFacetToObjectResponse where

instance NFData BatchAddFacetToObjectResponse where

-- | Represents the output of an 'AttachObject' operation.
--
--
--
-- /See:/ 'batchAttachObject' smart constructor.
data BatchAttachObject = BatchAttachObject'
  { _baoParentReference :: !ObjectReference
  , _baoChildReference  :: !ObjectReference
  , _baoLinkName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAttachObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baoParentReference' - The parent object reference.
--
-- * 'baoChildReference' - The child object reference that is to be attached to the object.
--
-- * 'baoLinkName' - The name of the link.
batchAttachObject
    :: ObjectReference -- ^ 'baoParentReference'
    -> ObjectReference -- ^ 'baoChildReference'
    -> Text -- ^ 'baoLinkName'
    -> BatchAttachObject
batchAttachObject pParentReference_ pChildReference_ pLinkName_ =
  BatchAttachObject'
    { _baoParentReference = pParentReference_
    , _baoChildReference = pChildReference_
    , _baoLinkName = pLinkName_
    }


-- | The parent object reference.
baoParentReference :: Lens' BatchAttachObject ObjectReference
baoParentReference = lens _baoParentReference (\ s a -> s{_baoParentReference = a})

-- | The child object reference that is to be attached to the object.
baoChildReference :: Lens' BatchAttachObject ObjectReference
baoChildReference = lens _baoChildReference (\ s a -> s{_baoChildReference = a})

-- | The name of the link.
baoLinkName :: Lens' BatchAttachObject Text
baoLinkName = lens _baoLinkName (\ s a -> s{_baoLinkName = a})

instance Hashable BatchAttachObject where

instance NFData BatchAttachObject where

instance ToJSON BatchAttachObject where
        toJSON BatchAttachObject'{..}
          = object
              (catMaybes
                 [Just ("ParentReference" .= _baoParentReference),
                  Just ("ChildReference" .= _baoChildReference),
                  Just ("LinkName" .= _baoLinkName)])

-- | Represents the output batch 'AttachObject' response operation.
--
--
--
-- /See:/ 'batchAttachObjectResponse' smart constructor.
newtype BatchAttachObjectResponse = BatchAttachObjectResponse'
  { _baoAttachedObjectIdentifier :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAttachObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baoAttachedObjectIdentifier' - The @ObjectIdentifier@ of the object that has been attached.
batchAttachObjectResponse
    :: BatchAttachObjectResponse
batchAttachObjectResponse =
  BatchAttachObjectResponse' {_baoAttachedObjectIdentifier = Nothing}


-- | The @ObjectIdentifier@ of the object that has been attached.
baoAttachedObjectIdentifier :: Lens' BatchAttachObjectResponse (Maybe Text)
baoAttachedObjectIdentifier = lens _baoAttachedObjectIdentifier (\ s a -> s{_baoAttachedObjectIdentifier = a})

instance FromJSON BatchAttachObjectResponse where
        parseJSON
          = withObject "BatchAttachObjectResponse"
              (\ x ->
                 BatchAttachObjectResponse' <$>
                   (x .:? "attachedObjectIdentifier"))

instance Hashable BatchAttachObjectResponse where

instance NFData BatchAttachObjectResponse where

-- | Attaches a policy object to a regular object inside a 'BatchRead' operation. For more information, see 'AttachPolicy' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchAttachPolicy' smart constructor.
data BatchAttachPolicy = BatchAttachPolicy'
  { _bapPolicyReference :: !ObjectReference
  , _bapObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAttachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bapPolicyReference' - The reference that is associated with the policy object.
--
-- * 'bapObjectReference' - The reference that identifies the object to which the policy will be attached.
batchAttachPolicy
    :: ObjectReference -- ^ 'bapPolicyReference'
    -> ObjectReference -- ^ 'bapObjectReference'
    -> BatchAttachPolicy
batchAttachPolicy pPolicyReference_ pObjectReference_ =
  BatchAttachPolicy'
    { _bapPolicyReference = pPolicyReference_
    , _bapObjectReference = pObjectReference_
    }


-- | The reference that is associated with the policy object.
bapPolicyReference :: Lens' BatchAttachPolicy ObjectReference
bapPolicyReference = lens _bapPolicyReference (\ s a -> s{_bapPolicyReference = a})

-- | The reference that identifies the object to which the policy will be attached.
bapObjectReference :: Lens' BatchAttachPolicy ObjectReference
bapObjectReference = lens _bapObjectReference (\ s a -> s{_bapObjectReference = a})

instance Hashable BatchAttachPolicy where

instance NFData BatchAttachPolicy where

instance ToJSON BatchAttachPolicy where
        toJSON BatchAttachPolicy'{..}
          = object
              (catMaybes
                 [Just ("PolicyReference" .= _bapPolicyReference),
                  Just ("ObjectReference" .= _bapObjectReference)])

-- | Represents the output of an 'AttachPolicy' response operation.
--
--
--
-- /See:/ 'batchAttachPolicyResponse' smart constructor.
data BatchAttachPolicyResponse =
  BatchAttachPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAttachPolicyResponse' with the minimum fields required to make a request.
--
batchAttachPolicyResponse
    :: BatchAttachPolicyResponse
batchAttachPolicyResponse = BatchAttachPolicyResponse'


instance FromJSON BatchAttachPolicyResponse where
        parseJSON
          = withObject "BatchAttachPolicyResponse"
              (\ x -> pure BatchAttachPolicyResponse')

instance Hashable BatchAttachPolicyResponse where

instance NFData BatchAttachPolicyResponse where

-- | Attaches the specified object to the specified index inside a 'BatchRead' operation. For more information, see 'AttachToIndex' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchAttachToIndex' smart constructor.
data BatchAttachToIndex = BatchAttachToIndex'
  { _batiIndexReference  :: !ObjectReference
  , _batiTargetReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAttachToIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batiIndexReference' - A reference to the index that you are attaching the object to.
--
-- * 'batiTargetReference' - A reference to the object that you are attaching to the index.
batchAttachToIndex
    :: ObjectReference -- ^ 'batiIndexReference'
    -> ObjectReference -- ^ 'batiTargetReference'
    -> BatchAttachToIndex
batchAttachToIndex pIndexReference_ pTargetReference_ =
  BatchAttachToIndex'
    { _batiIndexReference = pIndexReference_
    , _batiTargetReference = pTargetReference_
    }


-- | A reference to the index that you are attaching the object to.
batiIndexReference :: Lens' BatchAttachToIndex ObjectReference
batiIndexReference = lens _batiIndexReference (\ s a -> s{_batiIndexReference = a})

-- | A reference to the object that you are attaching to the index.
batiTargetReference :: Lens' BatchAttachToIndex ObjectReference
batiTargetReference = lens _batiTargetReference (\ s a -> s{_batiTargetReference = a})

instance Hashable BatchAttachToIndex where

instance NFData BatchAttachToIndex where

instance ToJSON BatchAttachToIndex where
        toJSON BatchAttachToIndex'{..}
          = object
              (catMaybes
                 [Just ("IndexReference" .= _batiIndexReference),
                  Just ("TargetReference" .= _batiTargetReference)])

-- | Represents the output of a 'AttachToIndex' response operation.
--
--
--
-- /See:/ 'batchAttachToIndexResponse' smart constructor.
newtype BatchAttachToIndexResponse = BatchAttachToIndexResponse'
  { _batiAttachedObjectIdentifier :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAttachToIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batiAttachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was attached to the index.
batchAttachToIndexResponse
    :: BatchAttachToIndexResponse
batchAttachToIndexResponse =
  BatchAttachToIndexResponse' {_batiAttachedObjectIdentifier = Nothing}


-- | The @ObjectIdentifier@ of the object that was attached to the index.
batiAttachedObjectIdentifier :: Lens' BatchAttachToIndexResponse (Maybe Text)
batiAttachedObjectIdentifier = lens _batiAttachedObjectIdentifier (\ s a -> s{_batiAttachedObjectIdentifier = a})

instance FromJSON BatchAttachToIndexResponse where
        parseJSON
          = withObject "BatchAttachToIndexResponse"
              (\ x ->
                 BatchAttachToIndexResponse' <$>
                   (x .:? "AttachedObjectIdentifier"))

instance Hashable BatchAttachToIndexResponse where

instance NFData BatchAttachToIndexResponse where

-- | Attaches a typed link to a specified source and target object inside a 'BatchRead' operation. For more information, see 'AttachTypedLink' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchAttachTypedLink' smart constructor.
data BatchAttachTypedLink = BatchAttachTypedLink'
  { _batlSourceObjectReference :: !ObjectReference
  , _batlTargetObjectReference :: !ObjectReference
  , _batlTypedLinkFacet        :: !TypedLinkSchemaAndFacetName
  , _batlAttributes            :: ![AttributeNameAndValue]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAttachTypedLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batlSourceObjectReference' - Identifies the source object that the typed link will attach to.
--
-- * 'batlTargetObjectReference' - Identifies the target object that the typed link will attach to.
--
-- * 'batlTypedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
--
-- * 'batlAttributes' - A set of attributes that are associated with the typed link.
batchAttachTypedLink
    :: ObjectReference -- ^ 'batlSourceObjectReference'
    -> ObjectReference -- ^ 'batlTargetObjectReference'
    -> TypedLinkSchemaAndFacetName -- ^ 'batlTypedLinkFacet'
    -> BatchAttachTypedLink
batchAttachTypedLink pSourceObjectReference_ pTargetObjectReference_ pTypedLinkFacet_ =
  BatchAttachTypedLink'
    { _batlSourceObjectReference = pSourceObjectReference_
    , _batlTargetObjectReference = pTargetObjectReference_
    , _batlTypedLinkFacet = pTypedLinkFacet_
    , _batlAttributes = mempty
    }


-- | Identifies the source object that the typed link will attach to.
batlSourceObjectReference :: Lens' BatchAttachTypedLink ObjectReference
batlSourceObjectReference = lens _batlSourceObjectReference (\ s a -> s{_batlSourceObjectReference = a})

-- | Identifies the target object that the typed link will attach to.
batlTargetObjectReference :: Lens' BatchAttachTypedLink ObjectReference
batlTargetObjectReference = lens _batlTargetObjectReference (\ s a -> s{_batlTargetObjectReference = a})

-- | Identifies the typed link facet that is associated with the typed link.
batlTypedLinkFacet :: Lens' BatchAttachTypedLink TypedLinkSchemaAndFacetName
batlTypedLinkFacet = lens _batlTypedLinkFacet (\ s a -> s{_batlTypedLinkFacet = a})

-- | A set of attributes that are associated with the typed link.
batlAttributes :: Lens' BatchAttachTypedLink [AttributeNameAndValue]
batlAttributes = lens _batlAttributes (\ s a -> s{_batlAttributes = a}) . _Coerce

instance Hashable BatchAttachTypedLink where

instance NFData BatchAttachTypedLink where

instance ToJSON BatchAttachTypedLink where
        toJSON BatchAttachTypedLink'{..}
          = object
              (catMaybes
                 [Just
                    ("SourceObjectReference" .=
                       _batlSourceObjectReference),
                  Just
                    ("TargetObjectReference" .=
                       _batlTargetObjectReference),
                  Just ("TypedLinkFacet" .= _batlTypedLinkFacet),
                  Just ("Attributes" .= _batlAttributes)])

-- | Represents the output of a 'AttachTypedLink' response operation.
--
--
--
-- /See:/ 'batchAttachTypedLinkResponse' smart constructor.
newtype BatchAttachTypedLinkResponse = BatchAttachTypedLinkResponse'
  { _batlTypedLinkSpecifier :: Maybe TypedLinkSpecifier
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchAttachTypedLinkResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batlTypedLinkSpecifier' - Returns a typed link specifier as output.
batchAttachTypedLinkResponse
    :: BatchAttachTypedLinkResponse
batchAttachTypedLinkResponse =
  BatchAttachTypedLinkResponse' {_batlTypedLinkSpecifier = Nothing}


-- | Returns a typed link specifier as output.
batlTypedLinkSpecifier :: Lens' BatchAttachTypedLinkResponse (Maybe TypedLinkSpecifier)
batlTypedLinkSpecifier = lens _batlTypedLinkSpecifier (\ s a -> s{_batlTypedLinkSpecifier = a})

instance FromJSON BatchAttachTypedLinkResponse where
        parseJSON
          = withObject "BatchAttachTypedLinkResponse"
              (\ x ->
                 BatchAttachTypedLinkResponse' <$>
                   (x .:? "TypedLinkSpecifier"))

instance Hashable BatchAttachTypedLinkResponse where

instance NFData BatchAttachTypedLinkResponse where

-- | Creates an index object inside of a 'BatchRead' operation. For more information, see 'CreateIndex' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchCreateIndex' smart constructor.
data BatchCreateIndex = BatchCreateIndex'
  { _bciParentReference             :: !(Maybe ObjectReference)
  , _bciLinkName                    :: !(Maybe Text)
  , _bciBatchReferenceName          :: !(Maybe Text)
  , _bciOrderedIndexedAttributeList :: ![AttributeKey]
  , _bciIsUnique                    :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchCreateIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bciParentReference' - A reference to the parent object that contains the index object.
--
-- * 'bciLinkName' - The name of the link between the parent object and the index object.
--
-- * 'bciBatchReferenceName' - The batch reference name. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#batches Batches> for more information.
--
-- * 'bciOrderedIndexedAttributeList' - Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
--
-- * 'bciIsUnique' - Indicates whether the attribute that is being indexed has unique values or not.
batchCreateIndex
    :: Bool -- ^ 'bciIsUnique'
    -> BatchCreateIndex
batchCreateIndex pIsUnique_ =
  BatchCreateIndex'
    { _bciParentReference = Nothing
    , _bciLinkName = Nothing
    , _bciBatchReferenceName = Nothing
    , _bciOrderedIndexedAttributeList = mempty
    , _bciIsUnique = pIsUnique_
    }


-- | A reference to the parent object that contains the index object.
bciParentReference :: Lens' BatchCreateIndex (Maybe ObjectReference)
bciParentReference = lens _bciParentReference (\ s a -> s{_bciParentReference = a})

-- | The name of the link between the parent object and the index object.
bciLinkName :: Lens' BatchCreateIndex (Maybe Text)
bciLinkName = lens _bciLinkName (\ s a -> s{_bciLinkName = a})

-- | The batch reference name. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#batches Batches> for more information.
bciBatchReferenceName :: Lens' BatchCreateIndex (Maybe Text)
bciBatchReferenceName = lens _bciBatchReferenceName (\ s a -> s{_bciBatchReferenceName = a})

-- | Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
bciOrderedIndexedAttributeList :: Lens' BatchCreateIndex [AttributeKey]
bciOrderedIndexedAttributeList = lens _bciOrderedIndexedAttributeList (\ s a -> s{_bciOrderedIndexedAttributeList = a}) . _Coerce

-- | Indicates whether the attribute that is being indexed has unique values or not.
bciIsUnique :: Lens' BatchCreateIndex Bool
bciIsUnique = lens _bciIsUnique (\ s a -> s{_bciIsUnique = a})

instance Hashable BatchCreateIndex where

instance NFData BatchCreateIndex where

instance ToJSON BatchCreateIndex where
        toJSON BatchCreateIndex'{..}
          = object
              (catMaybes
                 [("ParentReference" .=) <$> _bciParentReference,
                  ("LinkName" .=) <$> _bciLinkName,
                  ("BatchReferenceName" .=) <$> _bciBatchReferenceName,
                  Just
                    ("OrderedIndexedAttributeList" .=
                       _bciOrderedIndexedAttributeList),
                  Just ("IsUnique" .= _bciIsUnique)])

-- | Represents the output of a 'CreateIndex' response operation.
--
--
--
-- /See:/ 'batchCreateIndexResponse' smart constructor.
newtype BatchCreateIndexResponse = BatchCreateIndexResponse'
  { _bciObjectIdentifier :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchCreateIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bciObjectIdentifier' - The @ObjectIdentifier@ of the index created by this operation.
batchCreateIndexResponse
    :: BatchCreateIndexResponse
batchCreateIndexResponse =
  BatchCreateIndexResponse' {_bciObjectIdentifier = Nothing}


-- | The @ObjectIdentifier@ of the index created by this operation.
bciObjectIdentifier :: Lens' BatchCreateIndexResponse (Maybe Text)
bciObjectIdentifier = lens _bciObjectIdentifier (\ s a -> s{_bciObjectIdentifier = a})

instance FromJSON BatchCreateIndexResponse where
        parseJSON
          = withObject "BatchCreateIndexResponse"
              (\ x ->
                 BatchCreateIndexResponse' <$>
                   (x .:? "ObjectIdentifier"))

instance Hashable BatchCreateIndexResponse where

instance NFData BatchCreateIndexResponse where

-- | Represents the output of a 'CreateObject' operation.
--
--
--
-- /See:/ 'batchCreateObject' smart constructor.
data BatchCreateObject = BatchCreateObject'
  { _bcoParentReference     :: !(Maybe ObjectReference)
  , _bcoLinkName            :: !(Maybe Text)
  , _bcoBatchReferenceName  :: !(Maybe Text)
  , _bcoSchemaFacet         :: ![SchemaFacet]
  , _bcoObjectAttributeList :: ![AttributeKeyAndValue]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchCreateObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcoParentReference' - If specified, the parent reference to which this object will be attached.
--
-- * 'bcoLinkName' - The name of the link.
--
-- * 'bcoBatchReferenceName' - The batch reference name. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#batches Batches> for more information.
--
-- * 'bcoSchemaFacet' - A list of @FacetArns@ that will be associated with the object. For more information, see 'arns' .
--
-- * 'bcoObjectAttributeList' - An attribute map, which contains an attribute ARN as the key and attribute value as the map value.
batchCreateObject
    :: BatchCreateObject
batchCreateObject =
  BatchCreateObject'
    { _bcoParentReference = Nothing
    , _bcoLinkName = Nothing
    , _bcoBatchReferenceName = Nothing
    , _bcoSchemaFacet = mempty
    , _bcoObjectAttributeList = mempty
    }


-- | If specified, the parent reference to which this object will be attached.
bcoParentReference :: Lens' BatchCreateObject (Maybe ObjectReference)
bcoParentReference = lens _bcoParentReference (\ s a -> s{_bcoParentReference = a})

-- | The name of the link.
bcoLinkName :: Lens' BatchCreateObject (Maybe Text)
bcoLinkName = lens _bcoLinkName (\ s a -> s{_bcoLinkName = a})

-- | The batch reference name. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#batches Batches> for more information.
bcoBatchReferenceName :: Lens' BatchCreateObject (Maybe Text)
bcoBatchReferenceName = lens _bcoBatchReferenceName (\ s a -> s{_bcoBatchReferenceName = a})

-- | A list of @FacetArns@ that will be associated with the object. For more information, see 'arns' .
bcoSchemaFacet :: Lens' BatchCreateObject [SchemaFacet]
bcoSchemaFacet = lens _bcoSchemaFacet (\ s a -> s{_bcoSchemaFacet = a}) . _Coerce

-- | An attribute map, which contains an attribute ARN as the key and attribute value as the map value.
bcoObjectAttributeList :: Lens' BatchCreateObject [AttributeKeyAndValue]
bcoObjectAttributeList = lens _bcoObjectAttributeList (\ s a -> s{_bcoObjectAttributeList = a}) . _Coerce

instance Hashable BatchCreateObject where

instance NFData BatchCreateObject where

instance ToJSON BatchCreateObject where
        toJSON BatchCreateObject'{..}
          = object
              (catMaybes
                 [("ParentReference" .=) <$> _bcoParentReference,
                  ("LinkName" .=) <$> _bcoLinkName,
                  ("BatchReferenceName" .=) <$> _bcoBatchReferenceName,
                  Just ("SchemaFacet" .= _bcoSchemaFacet),
                  Just
                    ("ObjectAttributeList" .= _bcoObjectAttributeList)])

-- | Represents the output of a 'CreateObject' response operation.
--
--
--
-- /See:/ 'batchCreateObjectResponse' smart constructor.
newtype BatchCreateObjectResponse = BatchCreateObjectResponse'
  { _bcoObjectIdentifier :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchCreateObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcoObjectIdentifier' - The ID that is associated with the object.
batchCreateObjectResponse
    :: BatchCreateObjectResponse
batchCreateObjectResponse =
  BatchCreateObjectResponse' {_bcoObjectIdentifier = Nothing}


-- | The ID that is associated with the object.
bcoObjectIdentifier :: Lens' BatchCreateObjectResponse (Maybe Text)
bcoObjectIdentifier = lens _bcoObjectIdentifier (\ s a -> s{_bcoObjectIdentifier = a})

instance FromJSON BatchCreateObjectResponse where
        parseJSON
          = withObject "BatchCreateObjectResponse"
              (\ x ->
                 BatchCreateObjectResponse' <$>
                   (x .:? "ObjectIdentifier"))

instance Hashable BatchCreateObjectResponse where

instance NFData BatchCreateObjectResponse where

-- | Represents the output of a 'DeleteObject' operation.
--
--
--
-- /See:/ 'batchDeleteObject' smart constructor.
newtype BatchDeleteObject = BatchDeleteObject'
  { _bdoObjectReference :: ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdoObjectReference' - The reference that identifies the object.
batchDeleteObject
    :: ObjectReference -- ^ 'bdoObjectReference'
    -> BatchDeleteObject
batchDeleteObject pObjectReference_ =
  BatchDeleteObject' {_bdoObjectReference = pObjectReference_}


-- | The reference that identifies the object.
bdoObjectReference :: Lens' BatchDeleteObject ObjectReference
bdoObjectReference = lens _bdoObjectReference (\ s a -> s{_bdoObjectReference = a})

instance Hashable BatchDeleteObject where

instance NFData BatchDeleteObject where

instance ToJSON BatchDeleteObject where
        toJSON BatchDeleteObject'{..}
          = object
              (catMaybes
                 [Just ("ObjectReference" .= _bdoObjectReference)])

-- | Represents the output of a 'DeleteObject' response operation.
--
--
--
-- /See:/ 'batchDeleteObjectResponse' smart constructor.
data BatchDeleteObjectResponse =
  BatchDeleteObjectResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteObjectResponse' with the minimum fields required to make a request.
--
batchDeleteObjectResponse
    :: BatchDeleteObjectResponse
batchDeleteObjectResponse = BatchDeleteObjectResponse'


instance FromJSON BatchDeleteObjectResponse where
        parseJSON
          = withObject "BatchDeleteObjectResponse"
              (\ x -> pure BatchDeleteObjectResponse')

instance Hashable BatchDeleteObjectResponse where

instance NFData BatchDeleteObjectResponse where

-- | Detaches the specified object from the specified index inside a 'BatchRead' operation. For more information, see 'DetachFromIndex' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchDetachFromIndex' smart constructor.
data BatchDetachFromIndex = BatchDetachFromIndex'
  { _bdfiIndexReference  :: !ObjectReference
  , _bdfiTargetReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetachFromIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdfiIndexReference' - A reference to the index object.
--
-- * 'bdfiTargetReference' - A reference to the object being detached from the index.
batchDetachFromIndex
    :: ObjectReference -- ^ 'bdfiIndexReference'
    -> ObjectReference -- ^ 'bdfiTargetReference'
    -> BatchDetachFromIndex
batchDetachFromIndex pIndexReference_ pTargetReference_ =
  BatchDetachFromIndex'
    { _bdfiIndexReference = pIndexReference_
    , _bdfiTargetReference = pTargetReference_
    }


-- | A reference to the index object.
bdfiIndexReference :: Lens' BatchDetachFromIndex ObjectReference
bdfiIndexReference = lens _bdfiIndexReference (\ s a -> s{_bdfiIndexReference = a})

-- | A reference to the object being detached from the index.
bdfiTargetReference :: Lens' BatchDetachFromIndex ObjectReference
bdfiTargetReference = lens _bdfiTargetReference (\ s a -> s{_bdfiTargetReference = a})

instance Hashable BatchDetachFromIndex where

instance NFData BatchDetachFromIndex where

instance ToJSON BatchDetachFromIndex where
        toJSON BatchDetachFromIndex'{..}
          = object
              (catMaybes
                 [Just ("IndexReference" .= _bdfiIndexReference),
                  Just ("TargetReference" .= _bdfiTargetReference)])

-- | Represents the output of a 'DetachFromIndex' response operation.
--
--
--
-- /See:/ 'batchDetachFromIndexResponse' smart constructor.
newtype BatchDetachFromIndexResponse = BatchDetachFromIndexResponse'
  { _bdfiDetachedObjectIdentifier :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetachFromIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdfiDetachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was detached from the index.
batchDetachFromIndexResponse
    :: BatchDetachFromIndexResponse
batchDetachFromIndexResponse =
  BatchDetachFromIndexResponse' {_bdfiDetachedObjectIdentifier = Nothing}


-- | The @ObjectIdentifier@ of the object that was detached from the index.
bdfiDetachedObjectIdentifier :: Lens' BatchDetachFromIndexResponse (Maybe Text)
bdfiDetachedObjectIdentifier = lens _bdfiDetachedObjectIdentifier (\ s a -> s{_bdfiDetachedObjectIdentifier = a})

instance FromJSON BatchDetachFromIndexResponse where
        parseJSON
          = withObject "BatchDetachFromIndexResponse"
              (\ x ->
                 BatchDetachFromIndexResponse' <$>
                   (x .:? "DetachedObjectIdentifier"))

instance Hashable BatchDetachFromIndexResponse where

instance NFData BatchDetachFromIndexResponse where

-- | Represents the output of a 'DetachObject' operation.
--
--
--
-- /See:/ 'batchDetachObject' smart constructor.
data BatchDetachObject = BatchDetachObject'
  { _bdoBatchReferenceName :: !(Maybe Text)
  , _bdoParentReference    :: !ObjectReference
  , _bdoLinkName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetachObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdoBatchReferenceName' - The batch reference name. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#batches Batches> for more information.
--
-- * 'bdoParentReference' - Parent reference from which the object with the specified link name is detached.
--
-- * 'bdoLinkName' - The name of the link.
batchDetachObject
    :: ObjectReference -- ^ 'bdoParentReference'
    -> Text -- ^ 'bdoLinkName'
    -> BatchDetachObject
batchDetachObject pParentReference_ pLinkName_ =
  BatchDetachObject'
    { _bdoBatchReferenceName = Nothing
    , _bdoParentReference = pParentReference_
    , _bdoLinkName = pLinkName_
    }


-- | The batch reference name. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#batches Batches> for more information.
bdoBatchReferenceName :: Lens' BatchDetachObject (Maybe Text)
bdoBatchReferenceName = lens _bdoBatchReferenceName (\ s a -> s{_bdoBatchReferenceName = a})

-- | Parent reference from which the object with the specified link name is detached.
bdoParentReference :: Lens' BatchDetachObject ObjectReference
bdoParentReference = lens _bdoParentReference (\ s a -> s{_bdoParentReference = a})

-- | The name of the link.
bdoLinkName :: Lens' BatchDetachObject Text
bdoLinkName = lens _bdoLinkName (\ s a -> s{_bdoLinkName = a})

instance Hashable BatchDetachObject where

instance NFData BatchDetachObject where

instance ToJSON BatchDetachObject where
        toJSON BatchDetachObject'{..}
          = object
              (catMaybes
                 [("BatchReferenceName" .=) <$>
                    _bdoBatchReferenceName,
                  Just ("ParentReference" .= _bdoParentReference),
                  Just ("LinkName" .= _bdoLinkName)])

-- | Represents the output of a 'DetachObject' response operation.
--
--
--
-- /See:/ 'batchDetachObjectResponse' smart constructor.
newtype BatchDetachObjectResponse = BatchDetachObjectResponse'
  { _bdoDetachedObjectIdentifier :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetachObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdoDetachedObjectIdentifier' - The @ObjectIdentifier@ of the detached object.
batchDetachObjectResponse
    :: BatchDetachObjectResponse
batchDetachObjectResponse =
  BatchDetachObjectResponse' {_bdoDetachedObjectIdentifier = Nothing}


-- | The @ObjectIdentifier@ of the detached object.
bdoDetachedObjectIdentifier :: Lens' BatchDetachObjectResponse (Maybe Text)
bdoDetachedObjectIdentifier = lens _bdoDetachedObjectIdentifier (\ s a -> s{_bdoDetachedObjectIdentifier = a})

instance FromJSON BatchDetachObjectResponse where
        parseJSON
          = withObject "BatchDetachObjectResponse"
              (\ x ->
                 BatchDetachObjectResponse' <$>
                   (x .:? "detachedObjectIdentifier"))

instance Hashable BatchDetachObjectResponse where

instance NFData BatchDetachObjectResponse where

-- | Detaches the specified policy from the specified directory inside a 'BatchWrite' operation. For more information, see 'DetachPolicy' and 'BatchWriteRequest$Operations' .
--
--
--
-- /See:/ 'batchDetachPolicy' smart constructor.
data BatchDetachPolicy = BatchDetachPolicy'
  { _bdpPolicyReference :: !ObjectReference
  , _bdpObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetachPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdpPolicyReference' - Reference that identifies the policy object.
--
-- * 'bdpObjectReference' - Reference that identifies the object whose policy object will be detached.
batchDetachPolicy
    :: ObjectReference -- ^ 'bdpPolicyReference'
    -> ObjectReference -- ^ 'bdpObjectReference'
    -> BatchDetachPolicy
batchDetachPolicy pPolicyReference_ pObjectReference_ =
  BatchDetachPolicy'
    { _bdpPolicyReference = pPolicyReference_
    , _bdpObjectReference = pObjectReference_
    }


-- | Reference that identifies the policy object.
bdpPolicyReference :: Lens' BatchDetachPolicy ObjectReference
bdpPolicyReference = lens _bdpPolicyReference (\ s a -> s{_bdpPolicyReference = a})

-- | Reference that identifies the object whose policy object will be detached.
bdpObjectReference :: Lens' BatchDetachPolicy ObjectReference
bdpObjectReference = lens _bdpObjectReference (\ s a -> s{_bdpObjectReference = a})

instance Hashable BatchDetachPolicy where

instance NFData BatchDetachPolicy where

instance ToJSON BatchDetachPolicy where
        toJSON BatchDetachPolicy'{..}
          = object
              (catMaybes
                 [Just ("PolicyReference" .= _bdpPolicyReference),
                  Just ("ObjectReference" .= _bdpObjectReference)])

-- | Represents the output of a 'DetachPolicy' response operation.
--
--
--
-- /See:/ 'batchDetachPolicyResponse' smart constructor.
data BatchDetachPolicyResponse =
  BatchDetachPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetachPolicyResponse' with the minimum fields required to make a request.
--
batchDetachPolicyResponse
    :: BatchDetachPolicyResponse
batchDetachPolicyResponse = BatchDetachPolicyResponse'


instance FromJSON BatchDetachPolicyResponse where
        parseJSON
          = withObject "BatchDetachPolicyResponse"
              (\ x -> pure BatchDetachPolicyResponse')

instance Hashable BatchDetachPolicyResponse where

instance NFData BatchDetachPolicyResponse where

-- | Detaches a typed link from a specified source and target object inside a 'BatchRead' operation. For more information, see 'DetachTypedLink' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchDetachTypedLink' smart constructor.
newtype BatchDetachTypedLink = BatchDetachTypedLink'
  { _bdtlTypedLinkSpecifier :: TypedLinkSpecifier
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetachTypedLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdtlTypedLinkSpecifier' - Used to accept a typed link specifier as input.
batchDetachTypedLink
    :: TypedLinkSpecifier -- ^ 'bdtlTypedLinkSpecifier'
    -> BatchDetachTypedLink
batchDetachTypedLink pTypedLinkSpecifier_ =
  BatchDetachTypedLink' {_bdtlTypedLinkSpecifier = pTypedLinkSpecifier_}


-- | Used to accept a typed link specifier as input.
bdtlTypedLinkSpecifier :: Lens' BatchDetachTypedLink TypedLinkSpecifier
bdtlTypedLinkSpecifier = lens _bdtlTypedLinkSpecifier (\ s a -> s{_bdtlTypedLinkSpecifier = a})

instance Hashable BatchDetachTypedLink where

instance NFData BatchDetachTypedLink where

instance ToJSON BatchDetachTypedLink where
        toJSON BatchDetachTypedLink'{..}
          = object
              (catMaybes
                 [Just
                    ("TypedLinkSpecifier" .= _bdtlTypedLinkSpecifier)])

-- | Represents the output of a 'DetachTypedLink' response operation.
--
--
--
-- /See:/ 'batchDetachTypedLinkResponse' smart constructor.
data BatchDetachTypedLinkResponse =
  BatchDetachTypedLinkResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDetachTypedLinkResponse' with the minimum fields required to make a request.
--
batchDetachTypedLinkResponse
    :: BatchDetachTypedLinkResponse
batchDetachTypedLinkResponse = BatchDetachTypedLinkResponse'


instance FromJSON BatchDetachTypedLinkResponse where
        parseJSON
          = withObject "BatchDetachTypedLinkResponse"
              (\ x -> pure BatchDetachTypedLinkResponse')

instance Hashable BatchDetachTypedLinkResponse where

instance NFData BatchDetachTypedLinkResponse where

-- | Retrieves attributes within a facet that are associated with an object inside an 'BatchRead' operation. For more information, see 'GetObjectAttributes' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchGetObjectAttributes' smart constructor.
data BatchGetObjectAttributes = BatchGetObjectAttributes'
  { _bgoaObjectReference :: !ObjectReference
  , _bgoaSchemaFacet     :: !SchemaFacet
  , _bgoaAttributeNames  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetObjectAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgoaObjectReference' - Reference that identifies the object whose attributes will be retrieved.
--
-- * 'bgoaSchemaFacet' - Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
--
-- * 'bgoaAttributeNames' - List of attribute names whose values will be retrieved.
batchGetObjectAttributes
    :: ObjectReference -- ^ 'bgoaObjectReference'
    -> SchemaFacet -- ^ 'bgoaSchemaFacet'
    -> BatchGetObjectAttributes
batchGetObjectAttributes pObjectReference_ pSchemaFacet_ =
  BatchGetObjectAttributes'
    { _bgoaObjectReference = pObjectReference_
    , _bgoaSchemaFacet = pSchemaFacet_
    , _bgoaAttributeNames = mempty
    }


-- | Reference that identifies the object whose attributes will be retrieved.
bgoaObjectReference :: Lens' BatchGetObjectAttributes ObjectReference
bgoaObjectReference = lens _bgoaObjectReference (\ s a -> s{_bgoaObjectReference = a})

-- | Identifier for the facet whose attributes will be retrieved. See 'SchemaFacet' for details.
bgoaSchemaFacet :: Lens' BatchGetObjectAttributes SchemaFacet
bgoaSchemaFacet = lens _bgoaSchemaFacet (\ s a -> s{_bgoaSchemaFacet = a})

-- | List of attribute names whose values will be retrieved.
bgoaAttributeNames :: Lens' BatchGetObjectAttributes [Text]
bgoaAttributeNames = lens _bgoaAttributeNames (\ s a -> s{_bgoaAttributeNames = a}) . _Coerce

instance Hashable BatchGetObjectAttributes where

instance NFData BatchGetObjectAttributes where

instance ToJSON BatchGetObjectAttributes where
        toJSON BatchGetObjectAttributes'{..}
          = object
              (catMaybes
                 [Just ("ObjectReference" .= _bgoaObjectReference),
                  Just ("SchemaFacet" .= _bgoaSchemaFacet),
                  Just ("AttributeNames" .= _bgoaAttributeNames)])

-- | Represents the output of a 'GetObjectAttributes' response operation.
--
--
--
-- /See:/ 'batchGetObjectAttributesResponse' smart constructor.
newtype BatchGetObjectAttributesResponse = BatchGetObjectAttributesResponse'
  { _bgoaAttributes :: Maybe [AttributeKeyAndValue]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetObjectAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgoaAttributes' - The attribute values that are associated with an object.
batchGetObjectAttributesResponse
    :: BatchGetObjectAttributesResponse
batchGetObjectAttributesResponse =
  BatchGetObjectAttributesResponse' {_bgoaAttributes = Nothing}


-- | The attribute values that are associated with an object.
bgoaAttributes :: Lens' BatchGetObjectAttributesResponse [AttributeKeyAndValue]
bgoaAttributes = lens _bgoaAttributes (\ s a -> s{_bgoaAttributes = a}) . _Default . _Coerce

instance FromJSON BatchGetObjectAttributesResponse
         where
        parseJSON
          = withObject "BatchGetObjectAttributesResponse"
              (\ x ->
                 BatchGetObjectAttributesResponse' <$>
                   (x .:? "Attributes" .!= mempty))

instance Hashable BatchGetObjectAttributesResponse
         where

instance NFData BatchGetObjectAttributesResponse
         where

-- | Retrieves metadata about an object inside a 'BatchRead' operation. For more information, see 'GetObjectInformation' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchGetObjectInformation' smart constructor.
newtype BatchGetObjectInformation = BatchGetObjectInformation'
  { _bgoiObjectReference :: ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetObjectInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgoiObjectReference' - A reference to the object.
batchGetObjectInformation
    :: ObjectReference -- ^ 'bgoiObjectReference'
    -> BatchGetObjectInformation
batchGetObjectInformation pObjectReference_ =
  BatchGetObjectInformation' {_bgoiObjectReference = pObjectReference_}


-- | A reference to the object.
bgoiObjectReference :: Lens' BatchGetObjectInformation ObjectReference
bgoiObjectReference = lens _bgoiObjectReference (\ s a -> s{_bgoiObjectReference = a})

instance Hashable BatchGetObjectInformation where

instance NFData BatchGetObjectInformation where

instance ToJSON BatchGetObjectInformation where
        toJSON BatchGetObjectInformation'{..}
          = object
              (catMaybes
                 [Just ("ObjectReference" .= _bgoiObjectReference)])

-- | Represents the output of a 'GetObjectInformation' response operation.
--
--
--
-- /See:/ 'batchGetObjectInformationResponse' smart constructor.
data BatchGetObjectInformationResponse = BatchGetObjectInformationResponse'
  { _bgoiObjectIdentifier :: !(Maybe Text)
  , _bgoiSchemaFacets     :: !(Maybe [SchemaFacet])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetObjectInformationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgoiObjectIdentifier' - The @ObjectIdentifier@ of the specified object.
--
-- * 'bgoiSchemaFacets' - The facets attached to the specified object.
batchGetObjectInformationResponse
    :: BatchGetObjectInformationResponse
batchGetObjectInformationResponse =
  BatchGetObjectInformationResponse'
    {_bgoiObjectIdentifier = Nothing, _bgoiSchemaFacets = Nothing}


-- | The @ObjectIdentifier@ of the specified object.
bgoiObjectIdentifier :: Lens' BatchGetObjectInformationResponse (Maybe Text)
bgoiObjectIdentifier = lens _bgoiObjectIdentifier (\ s a -> s{_bgoiObjectIdentifier = a})

-- | The facets attached to the specified object.
bgoiSchemaFacets :: Lens' BatchGetObjectInformationResponse [SchemaFacet]
bgoiSchemaFacets = lens _bgoiSchemaFacets (\ s a -> s{_bgoiSchemaFacets = a}) . _Default . _Coerce

instance FromJSON BatchGetObjectInformationResponse
         where
        parseJSON
          = withObject "BatchGetObjectInformationResponse"
              (\ x ->
                 BatchGetObjectInformationResponse' <$>
                   (x .:? "ObjectIdentifier") <*>
                     (x .:? "SchemaFacets" .!= mempty))

instance Hashable BatchGetObjectInformationResponse
         where

instance NFData BatchGetObjectInformationResponse
         where

-- | Lists indices attached to an object inside a 'BatchRead' operation. For more information, see 'ListAttachedIndices' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListAttachedIndices' smart constructor.
data BatchListAttachedIndices = BatchListAttachedIndices'
  { _blaisNextToken       :: !(Maybe Text)
  , _blaisMaxResults      :: !(Maybe Nat)
  , _blaisTargetReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListAttachedIndices' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blaisNextToken' - The pagination token.
--
-- * 'blaisMaxResults' - The maximum number of results to retrieve.
--
-- * 'blaisTargetReference' - A reference to the object that has indices attached.
batchListAttachedIndices
    :: ObjectReference -- ^ 'blaisTargetReference'
    -> BatchListAttachedIndices
batchListAttachedIndices pTargetReference_ =
  BatchListAttachedIndices'
    { _blaisNextToken = Nothing
    , _blaisMaxResults = Nothing
    , _blaisTargetReference = pTargetReference_
    }


-- | The pagination token.
blaisNextToken :: Lens' BatchListAttachedIndices (Maybe Text)
blaisNextToken = lens _blaisNextToken (\ s a -> s{_blaisNextToken = a})

-- | The maximum number of results to retrieve.
blaisMaxResults :: Lens' BatchListAttachedIndices (Maybe Natural)
blaisMaxResults = lens _blaisMaxResults (\ s a -> s{_blaisMaxResults = a}) . mapping _Nat

-- | A reference to the object that has indices attached.
blaisTargetReference :: Lens' BatchListAttachedIndices ObjectReference
blaisTargetReference = lens _blaisTargetReference (\ s a -> s{_blaisTargetReference = a})

instance Hashable BatchListAttachedIndices where

instance NFData BatchListAttachedIndices where

instance ToJSON BatchListAttachedIndices where
        toJSON BatchListAttachedIndices'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _blaisNextToken,
                  ("MaxResults" .=) <$> _blaisMaxResults,
                  Just ("TargetReference" .= _blaisTargetReference)])

-- | Represents the output of a 'ListAttachedIndices' response operation.
--
--
--
-- /See:/ 'batchListAttachedIndicesResponse' smart constructor.
data BatchListAttachedIndicesResponse = BatchListAttachedIndicesResponse'
  { _blaiIndexAttachments :: !(Maybe [IndexAttachment])
  , _blaiNextToken        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListAttachedIndicesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blaiIndexAttachments' - The indices attached to the specified object.
--
-- * 'blaiNextToken' - The pagination token.
batchListAttachedIndicesResponse
    :: BatchListAttachedIndicesResponse
batchListAttachedIndicesResponse =
  BatchListAttachedIndicesResponse'
    {_blaiIndexAttachments = Nothing, _blaiNextToken = Nothing}


-- | The indices attached to the specified object.
blaiIndexAttachments :: Lens' BatchListAttachedIndicesResponse [IndexAttachment]
blaiIndexAttachments = lens _blaiIndexAttachments (\ s a -> s{_blaiIndexAttachments = a}) . _Default . _Coerce

-- | The pagination token.
blaiNextToken :: Lens' BatchListAttachedIndicesResponse (Maybe Text)
blaiNextToken = lens _blaiNextToken (\ s a -> s{_blaiNextToken = a})

instance FromJSON BatchListAttachedIndicesResponse
         where
        parseJSON
          = withObject "BatchListAttachedIndicesResponse"
              (\ x ->
                 BatchListAttachedIndicesResponse' <$>
                   (x .:? "IndexAttachments" .!= mempty) <*>
                     (x .:? "NextToken"))

instance Hashable BatchListAttachedIndicesResponse
         where

instance NFData BatchListAttachedIndicesResponse
         where

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object inside a 'BatchRead' operation. For more information, see 'ListIncomingTypedLinks' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListIncomingTypedLinks' smart constructor.
data BatchListIncomingTypedLinks = BatchListIncomingTypedLinks'
  { _blitlsFilterAttributeRanges :: !(Maybe [TypedLinkAttributeRange])
  , _blitlsNextToken             :: !(Maybe Text)
  , _blitlsFilterTypedLink       :: !(Maybe TypedLinkSchemaAndFacetName)
  , _blitlsMaxResults            :: !(Maybe Nat)
  , _blitlsObjectReference       :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListIncomingTypedLinks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blitlsFilterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- * 'blitlsNextToken' - The pagination token.
--
-- * 'blitlsFilterTypedLink' - Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
--
-- * 'blitlsMaxResults' - The maximum number of results to retrieve.
--
-- * 'blitlsObjectReference' - The reference that identifies the object whose attributes will be listed.
batchListIncomingTypedLinks
    :: ObjectReference -- ^ 'blitlsObjectReference'
    -> BatchListIncomingTypedLinks
batchListIncomingTypedLinks pObjectReference_ =
  BatchListIncomingTypedLinks'
    { _blitlsFilterAttributeRanges = Nothing
    , _blitlsNextToken = Nothing
    , _blitlsFilterTypedLink = Nothing
    , _blitlsMaxResults = Nothing
    , _blitlsObjectReference = pObjectReference_
    }


-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
blitlsFilterAttributeRanges :: Lens' BatchListIncomingTypedLinks [TypedLinkAttributeRange]
blitlsFilterAttributeRanges = lens _blitlsFilterAttributeRanges (\ s a -> s{_blitlsFilterAttributeRanges = a}) . _Default . _Coerce

-- | The pagination token.
blitlsNextToken :: Lens' BatchListIncomingTypedLinks (Maybe Text)
blitlsNextToken = lens _blitlsNextToken (\ s a -> s{_blitlsNextToken = a})

-- | Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls.
blitlsFilterTypedLink :: Lens' BatchListIncomingTypedLinks (Maybe TypedLinkSchemaAndFacetName)
blitlsFilterTypedLink = lens _blitlsFilterTypedLink (\ s a -> s{_blitlsFilterTypedLink = a})

-- | The maximum number of results to retrieve.
blitlsMaxResults :: Lens' BatchListIncomingTypedLinks (Maybe Natural)
blitlsMaxResults = lens _blitlsMaxResults (\ s a -> s{_blitlsMaxResults = a}) . mapping _Nat

-- | The reference that identifies the object whose attributes will be listed.
blitlsObjectReference :: Lens' BatchListIncomingTypedLinks ObjectReference
blitlsObjectReference = lens _blitlsObjectReference (\ s a -> s{_blitlsObjectReference = a})

instance Hashable BatchListIncomingTypedLinks where

instance NFData BatchListIncomingTypedLinks where

instance ToJSON BatchListIncomingTypedLinks where
        toJSON BatchListIncomingTypedLinks'{..}
          = object
              (catMaybes
                 [("FilterAttributeRanges" .=) <$>
                    _blitlsFilterAttributeRanges,
                  ("NextToken" .=) <$> _blitlsNextToken,
                  ("FilterTypedLink" .=) <$> _blitlsFilterTypedLink,
                  ("MaxResults" .=) <$> _blitlsMaxResults,
                  Just ("ObjectReference" .= _blitlsObjectReference)])

-- | Represents the output of a 'ListIncomingTypedLinks' response operation.
--
--
--
-- /See:/ 'batchListIncomingTypedLinksResponse' smart constructor.
data BatchListIncomingTypedLinksResponse = BatchListIncomingTypedLinksResponse'
  { _blitlLinkSpecifiers :: !(Maybe [TypedLinkSpecifier])
  , _blitlNextToken      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListIncomingTypedLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blitlLinkSpecifiers' - Returns one or more typed link specifiers as output.
--
-- * 'blitlNextToken' - The pagination token.
batchListIncomingTypedLinksResponse
    :: BatchListIncomingTypedLinksResponse
batchListIncomingTypedLinksResponse =
  BatchListIncomingTypedLinksResponse'
    {_blitlLinkSpecifiers = Nothing, _blitlNextToken = Nothing}


-- | Returns one or more typed link specifiers as output.
blitlLinkSpecifiers :: Lens' BatchListIncomingTypedLinksResponse [TypedLinkSpecifier]
blitlLinkSpecifiers = lens _blitlLinkSpecifiers (\ s a -> s{_blitlLinkSpecifiers = a}) . _Default . _Coerce

-- | The pagination token.
blitlNextToken :: Lens' BatchListIncomingTypedLinksResponse (Maybe Text)
blitlNextToken = lens _blitlNextToken (\ s a -> s{_blitlNextToken = a})

instance FromJSON BatchListIncomingTypedLinksResponse
         where
        parseJSON
          = withObject "BatchListIncomingTypedLinksResponse"
              (\ x ->
                 BatchListIncomingTypedLinksResponse' <$>
                   (x .:? "LinkSpecifiers" .!= mempty) <*>
                     (x .:? "NextToken"))

instance Hashable BatchListIncomingTypedLinksResponse
         where

instance NFData BatchListIncomingTypedLinksResponse
         where

-- | Lists objects attached to the specified index inside a 'BatchRead' operation. For more information, see 'ListIndex' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListIndex' smart constructor.
data BatchListIndex = BatchListIndex'
  { _batRangesOnIndexedValues :: !(Maybe [ObjectAttributeRange])
  , _batNextToken             :: !(Maybe Text)
  , _batMaxResults            :: !(Maybe Nat)
  , _batIndexReference        :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListIndex' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batRangesOnIndexedValues' - Specifies the ranges of indexed values that you want to query.
--
-- * 'batNextToken' - The pagination token.
--
-- * 'batMaxResults' - The maximum number of results to retrieve.
--
-- * 'batIndexReference' - The reference to the index to list.
batchListIndex
    :: ObjectReference -- ^ 'batIndexReference'
    -> BatchListIndex
batchListIndex pIndexReference_ =
  BatchListIndex'
    { _batRangesOnIndexedValues = Nothing
    , _batNextToken = Nothing
    , _batMaxResults = Nothing
    , _batIndexReference = pIndexReference_
    }


-- | Specifies the ranges of indexed values that you want to query.
batRangesOnIndexedValues :: Lens' BatchListIndex [ObjectAttributeRange]
batRangesOnIndexedValues = lens _batRangesOnIndexedValues (\ s a -> s{_batRangesOnIndexedValues = a}) . _Default . _Coerce

-- | The pagination token.
batNextToken :: Lens' BatchListIndex (Maybe Text)
batNextToken = lens _batNextToken (\ s a -> s{_batNextToken = a})

-- | The maximum number of results to retrieve.
batMaxResults :: Lens' BatchListIndex (Maybe Natural)
batMaxResults = lens _batMaxResults (\ s a -> s{_batMaxResults = a}) . mapping _Nat

-- | The reference to the index to list.
batIndexReference :: Lens' BatchListIndex ObjectReference
batIndexReference = lens _batIndexReference (\ s a -> s{_batIndexReference = a})

instance Hashable BatchListIndex where

instance NFData BatchListIndex where

instance ToJSON BatchListIndex where
        toJSON BatchListIndex'{..}
          = object
              (catMaybes
                 [("RangesOnIndexedValues" .=) <$>
                    _batRangesOnIndexedValues,
                  ("NextToken" .=) <$> _batNextToken,
                  ("MaxResults" .=) <$> _batMaxResults,
                  Just ("IndexReference" .= _batIndexReference)])

-- | Represents the output of a 'ListIndex' response operation.
--
--
--
-- /See:/ 'batchListIndexResponse' smart constructor.
data BatchListIndexResponse = BatchListIndexResponse'
  { _bliIndexAttachments :: !(Maybe [IndexAttachment])
  , _bliNextToken        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListIndexResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bliIndexAttachments' - The objects and indexed values attached to the index.
--
-- * 'bliNextToken' - The pagination token.
batchListIndexResponse
    :: BatchListIndexResponse
batchListIndexResponse =
  BatchListIndexResponse'
    {_bliIndexAttachments = Nothing, _bliNextToken = Nothing}


-- | The objects and indexed values attached to the index.
bliIndexAttachments :: Lens' BatchListIndexResponse [IndexAttachment]
bliIndexAttachments = lens _bliIndexAttachments (\ s a -> s{_bliIndexAttachments = a}) . _Default . _Coerce

-- | The pagination token.
bliNextToken :: Lens' BatchListIndexResponse (Maybe Text)
bliNextToken = lens _bliNextToken (\ s a -> s{_bliNextToken = a})

instance FromJSON BatchListIndexResponse where
        parseJSON
          = withObject "BatchListIndexResponse"
              (\ x ->
                 BatchListIndexResponse' <$>
                   (x .:? "IndexAttachments" .!= mempty) <*>
                     (x .:? "NextToken"))

instance Hashable BatchListIndexResponse where

instance NFData BatchListIndexResponse where

-- | Represents the output of a 'ListObjectAttributes' operation.
--
--
--
-- /See:/ 'batchListObjectAttributes' smart constructor.
data BatchListObjectAttributes = BatchListObjectAttributes'
  { _bloaFacetFilter     :: !(Maybe SchemaFacet)
  , _bloaNextToken       :: !(Maybe Text)
  , _bloaMaxResults      :: !(Maybe Nat)
  , _bloaObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListObjectAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bloaFacetFilter' - Used to filter the list of object attributes that are associated with a certain facet.
--
-- * 'bloaNextToken' - The pagination token.
--
-- * 'bloaMaxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'bloaObjectReference' - Reference of the object whose attributes need to be listed.
batchListObjectAttributes
    :: ObjectReference -- ^ 'bloaObjectReference'
    -> BatchListObjectAttributes
batchListObjectAttributes pObjectReference_ =
  BatchListObjectAttributes'
    { _bloaFacetFilter = Nothing
    , _bloaNextToken = Nothing
    , _bloaMaxResults = Nothing
    , _bloaObjectReference = pObjectReference_
    }


-- | Used to filter the list of object attributes that are associated with a certain facet.
bloaFacetFilter :: Lens' BatchListObjectAttributes (Maybe SchemaFacet)
bloaFacetFilter = lens _bloaFacetFilter (\ s a -> s{_bloaFacetFilter = a})

-- | The pagination token.
bloaNextToken :: Lens' BatchListObjectAttributes (Maybe Text)
bloaNextToken = lens _bloaNextToken (\ s a -> s{_bloaNextToken = a})

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
bloaMaxResults :: Lens' BatchListObjectAttributes (Maybe Natural)
bloaMaxResults = lens _bloaMaxResults (\ s a -> s{_bloaMaxResults = a}) . mapping _Nat

-- | Reference of the object whose attributes need to be listed.
bloaObjectReference :: Lens' BatchListObjectAttributes ObjectReference
bloaObjectReference = lens _bloaObjectReference (\ s a -> s{_bloaObjectReference = a})

instance Hashable BatchListObjectAttributes where

instance NFData BatchListObjectAttributes where

instance ToJSON BatchListObjectAttributes where
        toJSON BatchListObjectAttributes'{..}
          = object
              (catMaybes
                 [("FacetFilter" .=) <$> _bloaFacetFilter,
                  ("NextToken" .=) <$> _bloaNextToken,
                  ("MaxResults" .=) <$> _bloaMaxResults,
                  Just ("ObjectReference" .= _bloaObjectReference)])

-- | Represents the output of a 'ListObjectAttributes' response operation.
--
--
--
-- /See:/ 'batchListObjectAttributesResponse' smart constructor.
data BatchListObjectAttributesResponse = BatchListObjectAttributesResponse'
  { _bNextToken  :: !(Maybe Text)
  , _bAttributes :: !(Maybe [AttributeKeyAndValue])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListObjectAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bNextToken' - The pagination token.
--
-- * 'bAttributes' - The attributes map that is associated with the object. @AttributeArn@ is the key; attribute value is the value.
batchListObjectAttributesResponse
    :: BatchListObjectAttributesResponse
batchListObjectAttributesResponse =
  BatchListObjectAttributesResponse'
    {_bNextToken = Nothing, _bAttributes = Nothing}


-- | The pagination token.
bNextToken :: Lens' BatchListObjectAttributesResponse (Maybe Text)
bNextToken = lens _bNextToken (\ s a -> s{_bNextToken = a})

-- | The attributes map that is associated with the object. @AttributeArn@ is the key; attribute value is the value.
bAttributes :: Lens' BatchListObjectAttributesResponse [AttributeKeyAndValue]
bAttributes = lens _bAttributes (\ s a -> s{_bAttributes = a}) . _Default . _Coerce

instance FromJSON BatchListObjectAttributesResponse
         where
        parseJSON
          = withObject "BatchListObjectAttributesResponse"
              (\ x ->
                 BatchListObjectAttributesResponse' <$>
                   (x .:? "NextToken") <*>
                     (x .:? "Attributes" .!= mempty))

instance Hashable BatchListObjectAttributesResponse
         where

instance NFData BatchListObjectAttributesResponse
         where

-- | Represents the output of a 'ListObjectChildren' operation.
--
--
--
-- /See:/ 'batchListObjectChildren' smart constructor.
data BatchListObjectChildren = BatchListObjectChildren'
  { _bloclNextToken       :: !(Maybe Text)
  , _bloclMaxResults      :: !(Maybe Nat)
  , _bloclObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListObjectChildren' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bloclNextToken' - The pagination token.
--
-- * 'bloclMaxResults' - Maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'bloclObjectReference' - Reference of the object for which child objects are being listed.
batchListObjectChildren
    :: ObjectReference -- ^ 'bloclObjectReference'
    -> BatchListObjectChildren
batchListObjectChildren pObjectReference_ =
  BatchListObjectChildren'
    { _bloclNextToken = Nothing
    , _bloclMaxResults = Nothing
    , _bloclObjectReference = pObjectReference_
    }


-- | The pagination token.
bloclNextToken :: Lens' BatchListObjectChildren (Maybe Text)
bloclNextToken = lens _bloclNextToken (\ s a -> s{_bloclNextToken = a})

-- | Maximum number of items to be retrieved in a single call. This is an approximate number.
bloclMaxResults :: Lens' BatchListObjectChildren (Maybe Natural)
bloclMaxResults = lens _bloclMaxResults (\ s a -> s{_bloclMaxResults = a}) . mapping _Nat

-- | Reference of the object for which child objects are being listed.
bloclObjectReference :: Lens' BatchListObjectChildren ObjectReference
bloclObjectReference = lens _bloclObjectReference (\ s a -> s{_bloclObjectReference = a})

instance Hashable BatchListObjectChildren where

instance NFData BatchListObjectChildren where

instance ToJSON BatchListObjectChildren where
        toJSON BatchListObjectChildren'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _bloclNextToken,
                  ("MaxResults" .=) <$> _bloclMaxResults,
                  Just ("ObjectReference" .= _bloclObjectReference)])

-- | Represents the output of a 'ListObjectChildren' response operation.
--
--
--
-- /See:/ 'batchListObjectChildrenResponse' smart constructor.
data BatchListObjectChildrenResponse = BatchListObjectChildrenResponse'
  { _blocChildren  :: !(Maybe (Map Text Text))
  , _blocNextToken :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListObjectChildrenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blocChildren' - The children structure, which is a map with the key as the @LinkName@ and @ObjectIdentifier@ as the value.
--
-- * 'blocNextToken' - The pagination token.
batchListObjectChildrenResponse
    :: BatchListObjectChildrenResponse
batchListObjectChildrenResponse =
  BatchListObjectChildrenResponse'
    {_blocChildren = Nothing, _blocNextToken = Nothing}


-- | The children structure, which is a map with the key as the @LinkName@ and @ObjectIdentifier@ as the value.
blocChildren :: Lens' BatchListObjectChildrenResponse (HashMap Text Text)
blocChildren = lens _blocChildren (\ s a -> s{_blocChildren = a}) . _Default . _Map

-- | The pagination token.
blocNextToken :: Lens' BatchListObjectChildrenResponse (Maybe Text)
blocNextToken = lens _blocNextToken (\ s a -> s{_blocNextToken = a})

instance FromJSON BatchListObjectChildrenResponse
         where
        parseJSON
          = withObject "BatchListObjectChildrenResponse"
              (\ x ->
                 BatchListObjectChildrenResponse' <$>
                   (x .:? "Children" .!= mempty) <*>
                     (x .:? "NextToken"))

instance Hashable BatchListObjectChildrenResponse
         where

instance NFData BatchListObjectChildrenResponse where

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects inside a 'BatchRead' operation. For more information, see 'ListObjectParentPaths' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListObjectParentPaths' smart constructor.
data BatchListObjectParentPaths = BatchListObjectParentPaths'
  { _bloppsNextToken       :: !(Maybe Text)
  , _bloppsMaxResults      :: !(Maybe Nat)
  , _bloppsObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListObjectParentPaths' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bloppsNextToken' - The pagination token.
--
-- * 'bloppsMaxResults' - The maximum number of results to retrieve.
--
-- * 'bloppsObjectReference' - The reference that identifies the object whose attributes will be listed.
batchListObjectParentPaths
    :: ObjectReference -- ^ 'bloppsObjectReference'
    -> BatchListObjectParentPaths
batchListObjectParentPaths pObjectReference_ =
  BatchListObjectParentPaths'
    { _bloppsNextToken = Nothing
    , _bloppsMaxResults = Nothing
    , _bloppsObjectReference = pObjectReference_
    }


-- | The pagination token.
bloppsNextToken :: Lens' BatchListObjectParentPaths (Maybe Text)
bloppsNextToken = lens _bloppsNextToken (\ s a -> s{_bloppsNextToken = a})

-- | The maximum number of results to retrieve.
bloppsMaxResults :: Lens' BatchListObjectParentPaths (Maybe Natural)
bloppsMaxResults = lens _bloppsMaxResults (\ s a -> s{_bloppsMaxResults = a}) . mapping _Nat

-- | The reference that identifies the object whose attributes will be listed.
bloppsObjectReference :: Lens' BatchListObjectParentPaths ObjectReference
bloppsObjectReference = lens _bloppsObjectReference (\ s a -> s{_bloppsObjectReference = a})

instance Hashable BatchListObjectParentPaths where

instance NFData BatchListObjectParentPaths where

instance ToJSON BatchListObjectParentPaths where
        toJSON BatchListObjectParentPaths'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _bloppsNextToken,
                  ("MaxResults" .=) <$> _bloppsMaxResults,
                  Just ("ObjectReference" .= _bloppsObjectReference)])

-- | Represents the output of a 'ListObjectParentPaths' response operation.
--
--
--
-- /See:/ 'batchListObjectParentPathsResponse' smart constructor.
data BatchListObjectParentPathsResponse = BatchListObjectParentPathsResponse'
  { _bloppPathToObjectIdentifiersList :: !(Maybe [PathToObjectIdentifiers])
  , _bloppNextToken                   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListObjectParentPathsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bloppPathToObjectIdentifiersList' - Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
--
-- * 'bloppNextToken' - The pagination token.
batchListObjectParentPathsResponse
    :: BatchListObjectParentPathsResponse
batchListObjectParentPathsResponse =
  BatchListObjectParentPathsResponse'
    {_bloppPathToObjectIdentifiersList = Nothing, _bloppNextToken = Nothing}


-- | Returns the path to the @ObjectIdentifiers@ that are associated with the directory.
bloppPathToObjectIdentifiersList :: Lens' BatchListObjectParentPathsResponse [PathToObjectIdentifiers]
bloppPathToObjectIdentifiersList = lens _bloppPathToObjectIdentifiersList (\ s a -> s{_bloppPathToObjectIdentifiersList = a}) . _Default . _Coerce

-- | The pagination token.
bloppNextToken :: Lens' BatchListObjectParentPathsResponse (Maybe Text)
bloppNextToken = lens _bloppNextToken (\ s a -> s{_bloppNextToken = a})

instance FromJSON BatchListObjectParentPathsResponse
         where
        parseJSON
          = withObject "BatchListObjectParentPathsResponse"
              (\ x ->
                 BatchListObjectParentPathsResponse' <$>
                   (x .:? "PathToObjectIdentifiersList" .!= mempty) <*>
                     (x .:? "NextToken"))

instance Hashable BatchListObjectParentPathsResponse
         where

instance NFData BatchListObjectParentPathsResponse
         where

-- | Returns policies attached to an object in pagination fashion inside a 'BatchRead' operation. For more information, see 'ListObjectPolicies' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListObjectPolicies' smart constructor.
data BatchListObjectPolicies = BatchListObjectPolicies'
  { _blopsNextToken       :: !(Maybe Text)
  , _blopsMaxResults      :: !(Maybe Nat)
  , _blopsObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListObjectPolicies' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blopsNextToken' - The pagination token.
--
-- * 'blopsMaxResults' - The maximum number of results to retrieve.
--
-- * 'blopsObjectReference' - The reference that identifies the object whose attributes will be listed.
batchListObjectPolicies
    :: ObjectReference -- ^ 'blopsObjectReference'
    -> BatchListObjectPolicies
batchListObjectPolicies pObjectReference_ =
  BatchListObjectPolicies'
    { _blopsNextToken = Nothing
    , _blopsMaxResults = Nothing
    , _blopsObjectReference = pObjectReference_
    }


-- | The pagination token.
blopsNextToken :: Lens' BatchListObjectPolicies (Maybe Text)
blopsNextToken = lens _blopsNextToken (\ s a -> s{_blopsNextToken = a})

-- | The maximum number of results to retrieve.
blopsMaxResults :: Lens' BatchListObjectPolicies (Maybe Natural)
blopsMaxResults = lens _blopsMaxResults (\ s a -> s{_blopsMaxResults = a}) . mapping _Nat

-- | The reference that identifies the object whose attributes will be listed.
blopsObjectReference :: Lens' BatchListObjectPolicies ObjectReference
blopsObjectReference = lens _blopsObjectReference (\ s a -> s{_blopsObjectReference = a})

instance Hashable BatchListObjectPolicies where

instance NFData BatchListObjectPolicies where

instance ToJSON BatchListObjectPolicies where
        toJSON BatchListObjectPolicies'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _blopsNextToken,
                  ("MaxResults" .=) <$> _blopsMaxResults,
                  Just ("ObjectReference" .= _blopsObjectReference)])

-- | Represents the output of a 'ListObjectPolicies' response operation.
--
--
--
-- /See:/ 'batchListObjectPoliciesResponse' smart constructor.
data BatchListObjectPoliciesResponse = BatchListObjectPoliciesResponse'
  { _blopNextToken         :: !(Maybe Text)
  , _blopAttachedPolicyIds :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListObjectPoliciesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blopNextToken' - The pagination token.
--
-- * 'blopAttachedPolicyIds' - A list of policy @ObjectIdentifiers@ , that are attached to the object.
batchListObjectPoliciesResponse
    :: BatchListObjectPoliciesResponse
batchListObjectPoliciesResponse =
  BatchListObjectPoliciesResponse'
    {_blopNextToken = Nothing, _blopAttachedPolicyIds = Nothing}


-- | The pagination token.
blopNextToken :: Lens' BatchListObjectPoliciesResponse (Maybe Text)
blopNextToken = lens _blopNextToken (\ s a -> s{_blopNextToken = a})

-- | A list of policy @ObjectIdentifiers@ , that are attached to the object.
blopAttachedPolicyIds :: Lens' BatchListObjectPoliciesResponse [Text]
blopAttachedPolicyIds = lens _blopAttachedPolicyIds (\ s a -> s{_blopAttachedPolicyIds = a}) . _Default . _Coerce

instance FromJSON BatchListObjectPoliciesResponse
         where
        parseJSON
          = withObject "BatchListObjectPoliciesResponse"
              (\ x ->
                 BatchListObjectPoliciesResponse' <$>
                   (x .:? "NextToken") <*>
                     (x .:? "AttachedPolicyIds" .!= mempty))

instance Hashable BatchListObjectPoliciesResponse
         where

instance NFData BatchListObjectPoliciesResponse where

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object inside a 'BatchRead' operation. For more information, see 'ListOutgoingTypedLinks' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListOutgoingTypedLinks' smart constructor.
data BatchListOutgoingTypedLinks = BatchListOutgoingTypedLinks'
  { _blotlsFilterAttributeRanges :: !(Maybe [TypedLinkAttributeRange])
  , _blotlsNextToken             :: !(Maybe Text)
  , _blotlsFilterTypedLink       :: !(Maybe TypedLinkSchemaAndFacetName)
  , _blotlsMaxResults            :: !(Maybe Nat)
  , _blotlsObjectReference       :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListOutgoingTypedLinks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blotlsFilterAttributeRanges' - Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
--
-- * 'blotlsNextToken' - The pagination token.
--
-- * 'blotlsFilterTypedLink' - Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
--
-- * 'blotlsMaxResults' - The maximum number of results to retrieve.
--
-- * 'blotlsObjectReference' - The reference that identifies the object whose attributes will be listed.
batchListOutgoingTypedLinks
    :: ObjectReference -- ^ 'blotlsObjectReference'
    -> BatchListOutgoingTypedLinks
batchListOutgoingTypedLinks pObjectReference_ =
  BatchListOutgoingTypedLinks'
    { _blotlsFilterAttributeRanges = Nothing
    , _blotlsNextToken = Nothing
    , _blotlsFilterTypedLink = Nothing
    , _blotlsMaxResults = Nothing
    , _blotlsObjectReference = pObjectReference_
    }


-- | Provides range filters for multiple attributes. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range.
blotlsFilterAttributeRanges :: Lens' BatchListOutgoingTypedLinks [TypedLinkAttributeRange]
blotlsFilterAttributeRanges = lens _blotlsFilterAttributeRanges (\ s a -> s{_blotlsFilterAttributeRanges = a}) . _Default . _Coerce

-- | The pagination token.
blotlsNextToken :: Lens' BatchListOutgoingTypedLinks (Maybe Text)
blotlsNextToken = lens _blotlsNextToken (\ s a -> s{_blotlsNextToken = a})

-- | Filters are interpreted in the order of the attributes defined on the typed link facet, not the order they are supplied to any API calls.
blotlsFilterTypedLink :: Lens' BatchListOutgoingTypedLinks (Maybe TypedLinkSchemaAndFacetName)
blotlsFilterTypedLink = lens _blotlsFilterTypedLink (\ s a -> s{_blotlsFilterTypedLink = a})

-- | The maximum number of results to retrieve.
blotlsMaxResults :: Lens' BatchListOutgoingTypedLinks (Maybe Natural)
blotlsMaxResults = lens _blotlsMaxResults (\ s a -> s{_blotlsMaxResults = a}) . mapping _Nat

-- | The reference that identifies the object whose attributes will be listed.
blotlsObjectReference :: Lens' BatchListOutgoingTypedLinks ObjectReference
blotlsObjectReference = lens _blotlsObjectReference (\ s a -> s{_blotlsObjectReference = a})

instance Hashable BatchListOutgoingTypedLinks where

instance NFData BatchListOutgoingTypedLinks where

instance ToJSON BatchListOutgoingTypedLinks where
        toJSON BatchListOutgoingTypedLinks'{..}
          = object
              (catMaybes
                 [("FilterAttributeRanges" .=) <$>
                    _blotlsFilterAttributeRanges,
                  ("NextToken" .=) <$> _blotlsNextToken,
                  ("FilterTypedLink" .=) <$> _blotlsFilterTypedLink,
                  ("MaxResults" .=) <$> _blotlsMaxResults,
                  Just ("ObjectReference" .= _blotlsObjectReference)])

-- | Represents the output of a 'ListOutgoingTypedLinks' response operation.
--
--
--
-- /See:/ 'batchListOutgoingTypedLinksResponse' smart constructor.
data BatchListOutgoingTypedLinksResponse = BatchListOutgoingTypedLinksResponse'
  { _blotlTypedLinkSpecifiers :: !(Maybe [TypedLinkSpecifier])
  , _blotlNextToken           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListOutgoingTypedLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blotlTypedLinkSpecifiers' - Returns a typed link specifier as output.
--
-- * 'blotlNextToken' - The pagination token.
batchListOutgoingTypedLinksResponse
    :: BatchListOutgoingTypedLinksResponse
batchListOutgoingTypedLinksResponse =
  BatchListOutgoingTypedLinksResponse'
    {_blotlTypedLinkSpecifiers = Nothing, _blotlNextToken = Nothing}


-- | Returns a typed link specifier as output.
blotlTypedLinkSpecifiers :: Lens' BatchListOutgoingTypedLinksResponse [TypedLinkSpecifier]
blotlTypedLinkSpecifiers = lens _blotlTypedLinkSpecifiers (\ s a -> s{_blotlTypedLinkSpecifiers = a}) . _Default . _Coerce

-- | The pagination token.
blotlNextToken :: Lens' BatchListOutgoingTypedLinksResponse (Maybe Text)
blotlNextToken = lens _blotlNextToken (\ s a -> s{_blotlNextToken = a})

instance FromJSON BatchListOutgoingTypedLinksResponse
         where
        parseJSON
          = withObject "BatchListOutgoingTypedLinksResponse"
              (\ x ->
                 BatchListOutgoingTypedLinksResponse' <$>
                   (x .:? "TypedLinkSpecifiers" .!= mempty) <*>
                     (x .:? "NextToken"))

instance Hashable BatchListOutgoingTypedLinksResponse
         where

instance NFData BatchListOutgoingTypedLinksResponse
         where

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached inside a 'BatchRead' operation. For more information, see 'ListPolicyAttachments' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchListPolicyAttachments' smart constructor.
data BatchListPolicyAttachments = BatchListPolicyAttachments'
  { _blpasNextToken       :: !(Maybe Text)
  , _blpasMaxResults      :: !(Maybe Nat)
  , _blpasPolicyReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListPolicyAttachments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blpasNextToken' - The pagination token.
--
-- * 'blpasMaxResults' - The maximum number of results to retrieve.
--
-- * 'blpasPolicyReference' - The reference that identifies the policy object.
batchListPolicyAttachments
    :: ObjectReference -- ^ 'blpasPolicyReference'
    -> BatchListPolicyAttachments
batchListPolicyAttachments pPolicyReference_ =
  BatchListPolicyAttachments'
    { _blpasNextToken = Nothing
    , _blpasMaxResults = Nothing
    , _blpasPolicyReference = pPolicyReference_
    }


-- | The pagination token.
blpasNextToken :: Lens' BatchListPolicyAttachments (Maybe Text)
blpasNextToken = lens _blpasNextToken (\ s a -> s{_blpasNextToken = a})

-- | The maximum number of results to retrieve.
blpasMaxResults :: Lens' BatchListPolicyAttachments (Maybe Natural)
blpasMaxResults = lens _blpasMaxResults (\ s a -> s{_blpasMaxResults = a}) . mapping _Nat

-- | The reference that identifies the policy object.
blpasPolicyReference :: Lens' BatchListPolicyAttachments ObjectReference
blpasPolicyReference = lens _blpasPolicyReference (\ s a -> s{_blpasPolicyReference = a})

instance Hashable BatchListPolicyAttachments where

instance NFData BatchListPolicyAttachments where

instance ToJSON BatchListPolicyAttachments where
        toJSON BatchListPolicyAttachments'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _blpasNextToken,
                  ("MaxResults" .=) <$> _blpasMaxResults,
                  Just ("PolicyReference" .= _blpasPolicyReference)])

-- | Represents the output of a 'ListPolicyAttachments' response operation.
--
--
--
-- /See:/ 'batchListPolicyAttachmentsResponse' smart constructor.
data BatchListPolicyAttachmentsResponse = BatchListPolicyAttachmentsResponse'
  { _blpaObjectIdentifiers :: !(Maybe [Text])
  , _blpaNextToken         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchListPolicyAttachmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blpaObjectIdentifiers' - A list of @ObjectIdentifiers@ to which the policy is attached.
--
-- * 'blpaNextToken' - The pagination token.
batchListPolicyAttachmentsResponse
    :: BatchListPolicyAttachmentsResponse
batchListPolicyAttachmentsResponse =
  BatchListPolicyAttachmentsResponse'
    {_blpaObjectIdentifiers = Nothing, _blpaNextToken = Nothing}


-- | A list of @ObjectIdentifiers@ to which the policy is attached.
blpaObjectIdentifiers :: Lens' BatchListPolicyAttachmentsResponse [Text]
blpaObjectIdentifiers = lens _blpaObjectIdentifiers (\ s a -> s{_blpaObjectIdentifiers = a}) . _Default . _Coerce

-- | The pagination token.
blpaNextToken :: Lens' BatchListPolicyAttachmentsResponse (Maybe Text)
blpaNextToken = lens _blpaNextToken (\ s a -> s{_blpaNextToken = a})

instance FromJSON BatchListPolicyAttachmentsResponse
         where
        parseJSON
          = withObject "BatchListPolicyAttachmentsResponse"
              (\ x ->
                 BatchListPolicyAttachmentsResponse' <$>
                   (x .:? "ObjectIdentifiers" .!= mempty) <*>
                     (x .:? "NextToken"))

instance Hashable BatchListPolicyAttachmentsResponse
         where

instance NFData BatchListPolicyAttachmentsResponse
         where

-- | Lists all policies from the root of the Directory to the object specified inside a 'BatchRead' operation. For more information, see 'LookupPolicy' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchLookupPolicy' smart constructor.
data BatchLookupPolicy = BatchLookupPolicy'
  { _blplNextToken       :: !(Maybe Text)
  , _blplMaxResults      :: !(Maybe Nat)
  , _blplObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchLookupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blplNextToken' - The pagination token.
--
-- * 'blplMaxResults' - The maximum number of results to retrieve.
--
-- * 'blplObjectReference' - Reference that identifies the object whose policies will be looked up.
batchLookupPolicy
    :: ObjectReference -- ^ 'blplObjectReference'
    -> BatchLookupPolicy
batchLookupPolicy pObjectReference_ =
  BatchLookupPolicy'
    { _blplNextToken = Nothing
    , _blplMaxResults = Nothing
    , _blplObjectReference = pObjectReference_
    }


-- | The pagination token.
blplNextToken :: Lens' BatchLookupPolicy (Maybe Text)
blplNextToken = lens _blplNextToken (\ s a -> s{_blplNextToken = a})

-- | The maximum number of results to retrieve.
blplMaxResults :: Lens' BatchLookupPolicy (Maybe Natural)
blplMaxResults = lens _blplMaxResults (\ s a -> s{_blplMaxResults = a}) . mapping _Nat

-- | Reference that identifies the object whose policies will be looked up.
blplObjectReference :: Lens' BatchLookupPolicy ObjectReference
blplObjectReference = lens _blplObjectReference (\ s a -> s{_blplObjectReference = a})

instance Hashable BatchLookupPolicy where

instance NFData BatchLookupPolicy where

instance ToJSON BatchLookupPolicy where
        toJSON BatchLookupPolicy'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _blplNextToken,
                  ("MaxResults" .=) <$> _blplMaxResults,
                  Just ("ObjectReference" .= _blplObjectReference)])

-- | Represents the output of a 'LookupPolicy' response operation.
--
--
--
-- /See:/ 'batchLookupPolicyResponse' smart constructor.
data BatchLookupPolicyResponse = BatchLookupPolicyResponse'
  { _blpNextToken        :: !(Maybe Text)
  , _blpPolicyToPathList :: !(Maybe [PolicyToPath])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchLookupPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blpNextToken' - The pagination token.
--
-- * 'blpPolicyToPathList' - Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .
batchLookupPolicyResponse
    :: BatchLookupPolicyResponse
batchLookupPolicyResponse =
  BatchLookupPolicyResponse'
    {_blpNextToken = Nothing, _blpPolicyToPathList = Nothing}


-- | The pagination token.
blpNextToken :: Lens' BatchLookupPolicyResponse (Maybe Text)
blpNextToken = lens _blpNextToken (\ s a -> s{_blpNextToken = a})

-- | Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .
blpPolicyToPathList :: Lens' BatchLookupPolicyResponse [PolicyToPath]
blpPolicyToPathList = lens _blpPolicyToPathList (\ s a -> s{_blpPolicyToPathList = a}) . _Default . _Coerce

instance FromJSON BatchLookupPolicyResponse where
        parseJSON
          = withObject "BatchLookupPolicyResponse"
              (\ x ->
                 BatchLookupPolicyResponse' <$>
                   (x .:? "NextToken") <*>
                     (x .:? "PolicyToPathList" .!= mempty))

instance Hashable BatchLookupPolicyResponse where

instance NFData BatchLookupPolicyResponse where

-- | The batch read exception structure, which contains the exception type and message.
--
--
--
-- /See:/ 'batchReadException' smart constructor.
data BatchReadException = BatchReadException'
  { _breType    :: !(Maybe BatchReadExceptionType)
  , _breMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchReadException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'breType' - A type of exception, such as @InvalidArnException@ .
--
-- * 'breMessage' - An exception message that is associated with the failure.
batchReadException
    :: BatchReadException
batchReadException =
  BatchReadException' {_breType = Nothing, _breMessage = Nothing}


-- | A type of exception, such as @InvalidArnException@ .
breType :: Lens' BatchReadException (Maybe BatchReadExceptionType)
breType = lens _breType (\ s a -> s{_breType = a})

-- | An exception message that is associated with the failure.
breMessage :: Lens' BatchReadException (Maybe Text)
breMessage = lens _breMessage (\ s a -> s{_breMessage = a})

instance FromJSON BatchReadException where
        parseJSON
          = withObject "BatchReadException"
              (\ x ->
                 BatchReadException' <$>
                   (x .:? "Type") <*> (x .:? "Message"))

instance Hashable BatchReadException where

instance NFData BatchReadException where

-- | Represents the output of a @BatchRead@ operation.
--
--
--
-- /See:/ 'batchReadOperation' smart constructor.
data BatchReadOperation = BatchReadOperation'
  { _broListIndex              :: !(Maybe BatchListIndex)
  , _broGetObjectInformation   :: !(Maybe BatchGetObjectInformation)
  , _broListAttachedIndices    :: !(Maybe BatchListAttachedIndices)
  , _broLookupPolicy           :: !(Maybe BatchLookupPolicy)
  , _broListObjectParentPaths  :: !(Maybe BatchListObjectParentPaths)
  , _broListObjectAttributes   :: !(Maybe BatchListObjectAttributes)
  , _broListIncomingTypedLinks :: !(Maybe BatchListIncomingTypedLinks)
  , _broGetObjectAttributes    :: !(Maybe BatchGetObjectAttributes)
  , _broListObjectChildren     :: !(Maybe BatchListObjectChildren)
  , _broListPolicyAttachments  :: !(Maybe BatchListPolicyAttachments)
  , _broListOutgoingTypedLinks :: !(Maybe BatchListOutgoingTypedLinks)
  , _broListObjectPolicies     :: !(Maybe BatchListObjectPolicies)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchReadOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'broListIndex' - Lists objects attached to the specified index.
--
-- * 'broGetObjectInformation' - Retrieves metadata about an object.
--
-- * 'broListAttachedIndices' - Lists indices attached to an object.
--
-- * 'broLookupPolicy' - Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .
--
-- * 'broListObjectParentPaths' - Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#dirstructure Directory Structure> .
--
-- * 'broListObjectAttributes' - Lists all attributes that are associated with an object.
--
-- * 'broListIncomingTypedLinks' - Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
-- * 'broGetObjectAttributes' - Retrieves attributes within a facet that are associated with an object.
--
-- * 'broListObjectChildren' - Returns a paginated list of child objects that are associated with a given object.
--
-- * 'broListPolicyAttachments' - Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- * 'broListOutgoingTypedLinks' - Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
-- * 'broListObjectPolicies' - Returns policies attached to an object in pagination fashion.
batchReadOperation
    :: BatchReadOperation
batchReadOperation =
  BatchReadOperation'
    { _broListIndex = Nothing
    , _broGetObjectInformation = Nothing
    , _broListAttachedIndices = Nothing
    , _broLookupPolicy = Nothing
    , _broListObjectParentPaths = Nothing
    , _broListObjectAttributes = Nothing
    , _broListIncomingTypedLinks = Nothing
    , _broGetObjectAttributes = Nothing
    , _broListObjectChildren = Nothing
    , _broListPolicyAttachments = Nothing
    , _broListOutgoingTypedLinks = Nothing
    , _broListObjectPolicies = Nothing
    }


-- | Lists objects attached to the specified index.
broListIndex :: Lens' BatchReadOperation (Maybe BatchListIndex)
broListIndex = lens _broListIndex (\ s a -> s{_broListIndex = a})

-- | Retrieves metadata about an object.
broGetObjectInformation :: Lens' BatchReadOperation (Maybe BatchGetObjectInformation)
broGetObjectInformation = lens _broGetObjectInformation (\ s a -> s{_broGetObjectInformation = a})

-- | Lists indices attached to an object.
broListAttachedIndices :: Lens' BatchReadOperation (Maybe BatchListAttachedIndices)
broListAttachedIndices = lens _broListAttachedIndices (\ s a -> s{_broListAttachedIndices = a})

-- | Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .
broLookupPolicy :: Lens' BatchReadOperation (Maybe BatchLookupPolicy)
broLookupPolicy = lens _broLookupPolicy (\ s a -> s{_broLookupPolicy = a})

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#dirstructure Directory Structure> .
broListObjectParentPaths :: Lens' BatchReadOperation (Maybe BatchListObjectParentPaths)
broListObjectParentPaths = lens _broListObjectParentPaths (\ s a -> s{_broListObjectParentPaths = a})

-- | Lists all attributes that are associated with an object.
broListObjectAttributes :: Lens' BatchReadOperation (Maybe BatchListObjectAttributes)
broListObjectAttributes = lens _broListObjectAttributes (\ s a -> s{_broListObjectAttributes = a})

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
broListIncomingTypedLinks :: Lens' BatchReadOperation (Maybe BatchListIncomingTypedLinks)
broListIncomingTypedLinks = lens _broListIncomingTypedLinks (\ s a -> s{_broListIncomingTypedLinks = a})

-- | Retrieves attributes within a facet that are associated with an object.
broGetObjectAttributes :: Lens' BatchReadOperation (Maybe BatchGetObjectAttributes)
broGetObjectAttributes = lens _broGetObjectAttributes (\ s a -> s{_broGetObjectAttributes = a})

-- | Returns a paginated list of child objects that are associated with a given object.
broListObjectChildren :: Lens' BatchReadOperation (Maybe BatchListObjectChildren)
broListObjectChildren = lens _broListObjectChildren (\ s a -> s{_broListObjectChildren = a})

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
broListPolicyAttachments :: Lens' BatchReadOperation (Maybe BatchListPolicyAttachments)
broListPolicyAttachments = lens _broListPolicyAttachments (\ s a -> s{_broListPolicyAttachments = a})

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
broListOutgoingTypedLinks :: Lens' BatchReadOperation (Maybe BatchListOutgoingTypedLinks)
broListOutgoingTypedLinks = lens _broListOutgoingTypedLinks (\ s a -> s{_broListOutgoingTypedLinks = a})

-- | Returns policies attached to an object in pagination fashion.
broListObjectPolicies :: Lens' BatchReadOperation (Maybe BatchListObjectPolicies)
broListObjectPolicies = lens _broListObjectPolicies (\ s a -> s{_broListObjectPolicies = a})

instance Hashable BatchReadOperation where

instance NFData BatchReadOperation where

instance ToJSON BatchReadOperation where
        toJSON BatchReadOperation'{..}
          = object
              (catMaybes
                 [("ListIndex" .=) <$> _broListIndex,
                  ("GetObjectInformation" .=) <$>
                    _broGetObjectInformation,
                  ("ListAttachedIndices" .=) <$>
                    _broListAttachedIndices,
                  ("LookupPolicy" .=) <$> _broLookupPolicy,
                  ("ListObjectParentPaths" .=) <$>
                    _broListObjectParentPaths,
                  ("ListObjectAttributes" .=) <$>
                    _broListObjectAttributes,
                  ("ListIncomingTypedLinks" .=) <$>
                    _broListIncomingTypedLinks,
                  ("GetObjectAttributes" .=) <$>
                    _broGetObjectAttributes,
                  ("ListObjectChildren" .=) <$> _broListObjectChildren,
                  ("ListPolicyAttachments" .=) <$>
                    _broListPolicyAttachments,
                  ("ListOutgoingTypedLinks" .=) <$>
                    _broListOutgoingTypedLinks,
                  ("ListObjectPolicies" .=) <$>
                    _broListObjectPolicies])

-- | Represents the output of a @BatchRead@ response operation.
--
--
--
-- /See:/ 'batchReadOperationResponse' smart constructor.
data BatchReadOperationResponse = BatchReadOperationResponse'
  { _broExceptionResponse  :: !(Maybe BatchReadException)
  , _broSuccessfulResponse :: !(Maybe BatchReadSuccessfulResponse)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchReadOperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'broExceptionResponse' - Identifies which operation in a batch has failed.
--
-- * 'broSuccessfulResponse' - Identifies which operation in a batch has succeeded.
batchReadOperationResponse
    :: BatchReadOperationResponse
batchReadOperationResponse =
  BatchReadOperationResponse'
    {_broExceptionResponse = Nothing, _broSuccessfulResponse = Nothing}


-- | Identifies which operation in a batch has failed.
broExceptionResponse :: Lens' BatchReadOperationResponse (Maybe BatchReadException)
broExceptionResponse = lens _broExceptionResponse (\ s a -> s{_broExceptionResponse = a})

-- | Identifies which operation in a batch has succeeded.
broSuccessfulResponse :: Lens' BatchReadOperationResponse (Maybe BatchReadSuccessfulResponse)
broSuccessfulResponse = lens _broSuccessfulResponse (\ s a -> s{_broSuccessfulResponse = a})

instance FromJSON BatchReadOperationResponse where
        parseJSON
          = withObject "BatchReadOperationResponse"
              (\ x ->
                 BatchReadOperationResponse' <$>
                   (x .:? "ExceptionResponse") <*>
                     (x .:? "SuccessfulResponse"))

instance Hashable BatchReadOperationResponse where

instance NFData BatchReadOperationResponse where

-- | Represents the output of a @BatchRead@ success response operation.
--
--
--
-- /See:/ 'batchReadSuccessfulResponse' smart constructor.
data BatchReadSuccessfulResponse = BatchReadSuccessfulResponse'
  { _brsListIndex              :: !(Maybe BatchListIndexResponse)
  , _brsGetObjectInformation   :: !(Maybe BatchGetObjectInformationResponse)
  , _brsListAttachedIndices    :: !(Maybe BatchListAttachedIndicesResponse)
  , _brsLookupPolicy           :: !(Maybe BatchLookupPolicyResponse)
  , _brsListObjectParentPaths  :: !(Maybe BatchListObjectParentPathsResponse)
  , _brsListObjectAttributes   :: !(Maybe BatchListObjectAttributesResponse)
  , _brsListIncomingTypedLinks :: !(Maybe BatchListIncomingTypedLinksResponse)
  , _brsGetObjectAttributes    :: !(Maybe BatchGetObjectAttributesResponse)
  , _brsListObjectChildren     :: !(Maybe BatchListObjectChildrenResponse)
  , _brsListPolicyAttachments  :: !(Maybe BatchListPolicyAttachmentsResponse)
  , _brsListOutgoingTypedLinks :: !(Maybe BatchListOutgoingTypedLinksResponse)
  , _brsListObjectPolicies     :: !(Maybe BatchListObjectPoliciesResponse)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchReadSuccessfulResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsListIndex' - Lists objects attached to the specified index.
--
-- * 'brsGetObjectInformation' - Retrieves metadata about an object.
--
-- * 'brsListAttachedIndices' - Lists indices attached to an object.
--
-- * 'brsLookupPolicy' - Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .
--
-- * 'brsListObjectParentPaths' - Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#dirstructure Directory Structure> .
--
-- * 'brsListObjectAttributes' - Lists all attributes that are associated with an object.
--
-- * 'brsListIncomingTypedLinks' - Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
-- * 'brsGetObjectAttributes' - Retrieves attributes within a facet that are associated with an object.
--
-- * 'brsListObjectChildren' - Returns a paginated list of child objects that are associated with a given object.
--
-- * 'brsListPolicyAttachments' - Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
-- * 'brsListOutgoingTypedLinks' - Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
-- * 'brsListObjectPolicies' - Returns policies attached to an object in pagination fashion.
batchReadSuccessfulResponse
    :: BatchReadSuccessfulResponse
batchReadSuccessfulResponse =
  BatchReadSuccessfulResponse'
    { _brsListIndex = Nothing
    , _brsGetObjectInformation = Nothing
    , _brsListAttachedIndices = Nothing
    , _brsLookupPolicy = Nothing
    , _brsListObjectParentPaths = Nothing
    , _brsListObjectAttributes = Nothing
    , _brsListIncomingTypedLinks = Nothing
    , _brsGetObjectAttributes = Nothing
    , _brsListObjectChildren = Nothing
    , _brsListPolicyAttachments = Nothing
    , _brsListOutgoingTypedLinks = Nothing
    , _brsListObjectPolicies = Nothing
    }


-- | Lists objects attached to the specified index.
brsListIndex :: Lens' BatchReadSuccessfulResponse (Maybe BatchListIndexResponse)
brsListIndex = lens _brsListIndex (\ s a -> s{_brsListIndex = a})

-- | Retrieves metadata about an object.
brsGetObjectInformation :: Lens' BatchReadSuccessfulResponse (Maybe BatchGetObjectInformationResponse)
brsGetObjectInformation = lens _brsGetObjectInformation (\ s a -> s{_brsGetObjectInformation = a})

-- | Lists indices attached to an object.
brsListAttachedIndices :: Lens' BatchReadSuccessfulResponse (Maybe BatchListAttachedIndicesResponse)
brsListAttachedIndices = lens _brsListAttachedIndices (\ s a -> s{_brsListAttachedIndices = a})

-- | Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .
brsLookupPolicy :: Lens' BatchReadSuccessfulResponse (Maybe BatchLookupPolicyResponse)
brsLookupPolicy = lens _brsLookupPolicy (\ s a -> s{_brsLookupPolicy = a})

-- | Retrieves all available parent paths for any object type such as node, leaf node, policy node, and index node objects. For more information about objects, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#dirstructure Directory Structure> .
brsListObjectParentPaths :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectParentPathsResponse)
brsListObjectParentPaths = lens _brsListObjectParentPaths (\ s a -> s{_brsListObjectParentPaths = a})

-- | Lists all attributes that are associated with an object.
brsListObjectAttributes :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectAttributesResponse)
brsListObjectAttributes = lens _brsListObjectAttributes (\ s a -> s{_brsListObjectAttributes = a})

-- | Returns a paginated list of all the incoming 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
brsListIncomingTypedLinks :: Lens' BatchReadSuccessfulResponse (Maybe BatchListIncomingTypedLinksResponse)
brsListIncomingTypedLinks = lens _brsListIncomingTypedLinks (\ s a -> s{_brsListIncomingTypedLinks = a})

-- | Retrieves attributes within a facet that are associated with an object.
brsGetObjectAttributes :: Lens' BatchReadSuccessfulResponse (Maybe BatchGetObjectAttributesResponse)
brsGetObjectAttributes = lens _brsGetObjectAttributes (\ s a -> s{_brsGetObjectAttributes = a})

-- | Returns a paginated list of child objects that are associated with a given object.
brsListObjectChildren :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectChildrenResponse)
brsListObjectChildren = lens _brsListObjectChildren (\ s a -> s{_brsListObjectChildren = a})

-- | Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
brsListPolicyAttachments :: Lens' BatchReadSuccessfulResponse (Maybe BatchListPolicyAttachmentsResponse)
brsListPolicyAttachments = lens _brsListPolicyAttachments (\ s a -> s{_brsListPolicyAttachments = a})

-- | Returns a paginated list of all the outgoing 'TypedLinkSpecifier' information for an object. It also supports filtering by typed link facet and identity attributes. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
brsListOutgoingTypedLinks :: Lens' BatchReadSuccessfulResponse (Maybe BatchListOutgoingTypedLinksResponse)
brsListOutgoingTypedLinks = lens _brsListOutgoingTypedLinks (\ s a -> s{_brsListOutgoingTypedLinks = a})

-- | Returns policies attached to an object in pagination fashion.
brsListObjectPolicies :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectPoliciesResponse)
brsListObjectPolicies = lens _brsListObjectPolicies (\ s a -> s{_brsListObjectPolicies = a})

instance FromJSON BatchReadSuccessfulResponse where
        parseJSON
          = withObject "BatchReadSuccessfulResponse"
              (\ x ->
                 BatchReadSuccessfulResponse' <$>
                   (x .:? "ListIndex") <*>
                     (x .:? "GetObjectInformation")
                     <*> (x .:? "ListAttachedIndices")
                     <*> (x .:? "LookupPolicy")
                     <*> (x .:? "ListObjectParentPaths")
                     <*> (x .:? "ListObjectAttributes")
                     <*> (x .:? "ListIncomingTypedLinks")
                     <*> (x .:? "GetObjectAttributes")
                     <*> (x .:? "ListObjectChildren")
                     <*> (x .:? "ListPolicyAttachments")
                     <*> (x .:? "ListOutgoingTypedLinks")
                     <*> (x .:? "ListObjectPolicies"))

instance Hashable BatchReadSuccessfulResponse where

instance NFData BatchReadSuccessfulResponse where

-- | A batch operation to remove a facet from an object.
--
--
--
-- /See:/ 'batchRemoveFacetFromObject' smart constructor.
data BatchRemoveFacetFromObject = BatchRemoveFacetFromObject'
  { _brffoSchemaFacet     :: !SchemaFacet
  , _brffoObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchRemoveFacetFromObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brffoSchemaFacet' - The facet to remove from the object.
--
-- * 'brffoObjectReference' - A reference to the object whose facet will be removed.
batchRemoveFacetFromObject
    :: SchemaFacet -- ^ 'brffoSchemaFacet'
    -> ObjectReference -- ^ 'brffoObjectReference'
    -> BatchRemoveFacetFromObject
batchRemoveFacetFromObject pSchemaFacet_ pObjectReference_ =
  BatchRemoveFacetFromObject'
    { _brffoSchemaFacet = pSchemaFacet_
    , _brffoObjectReference = pObjectReference_
    }


-- | The facet to remove from the object.
brffoSchemaFacet :: Lens' BatchRemoveFacetFromObject SchemaFacet
brffoSchemaFacet = lens _brffoSchemaFacet (\ s a -> s{_brffoSchemaFacet = a})

-- | A reference to the object whose facet will be removed.
brffoObjectReference :: Lens' BatchRemoveFacetFromObject ObjectReference
brffoObjectReference = lens _brffoObjectReference (\ s a -> s{_brffoObjectReference = a})

instance Hashable BatchRemoveFacetFromObject where

instance NFData BatchRemoveFacetFromObject where

instance ToJSON BatchRemoveFacetFromObject where
        toJSON BatchRemoveFacetFromObject'{..}
          = object
              (catMaybes
                 [Just ("SchemaFacet" .= _brffoSchemaFacet),
                  Just ("ObjectReference" .= _brffoObjectReference)])

-- | An empty result that represents success.
--
--
--
-- /See:/ 'batchRemoveFacetFromObjectResponse' smart constructor.
data BatchRemoveFacetFromObjectResponse =
  BatchRemoveFacetFromObjectResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchRemoveFacetFromObjectResponse' with the minimum fields required to make a request.
--
batchRemoveFacetFromObjectResponse
    :: BatchRemoveFacetFromObjectResponse
batchRemoveFacetFromObjectResponse = BatchRemoveFacetFromObjectResponse'


instance FromJSON BatchRemoveFacetFromObjectResponse
         where
        parseJSON
          = withObject "BatchRemoveFacetFromObjectResponse"
              (\ x -> pure BatchRemoveFacetFromObjectResponse')

instance Hashable BatchRemoveFacetFromObjectResponse
         where

instance NFData BatchRemoveFacetFromObjectResponse
         where

-- | Represents the output of a @BatchUpdate@ operation.
--
--
--
-- /See:/ 'batchUpdateObjectAttributes' smart constructor.
data BatchUpdateObjectAttributes = BatchUpdateObjectAttributes'
  { _buoaObjectReference  :: !ObjectReference
  , _buoaAttributeUpdates :: ![ObjectAttributeUpdate]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchUpdateObjectAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'buoaObjectReference' - Reference that identifies the object.
--
-- * 'buoaAttributeUpdates' - Attributes update structure.
batchUpdateObjectAttributes
    :: ObjectReference -- ^ 'buoaObjectReference'
    -> BatchUpdateObjectAttributes
batchUpdateObjectAttributes pObjectReference_ =
  BatchUpdateObjectAttributes'
    {_buoaObjectReference = pObjectReference_, _buoaAttributeUpdates = mempty}


-- | Reference that identifies the object.
buoaObjectReference :: Lens' BatchUpdateObjectAttributes ObjectReference
buoaObjectReference = lens _buoaObjectReference (\ s a -> s{_buoaObjectReference = a})

-- | Attributes update structure.
buoaAttributeUpdates :: Lens' BatchUpdateObjectAttributes [ObjectAttributeUpdate]
buoaAttributeUpdates = lens _buoaAttributeUpdates (\ s a -> s{_buoaAttributeUpdates = a}) . _Coerce

instance Hashable BatchUpdateObjectAttributes where

instance NFData BatchUpdateObjectAttributes where

instance ToJSON BatchUpdateObjectAttributes where
        toJSON BatchUpdateObjectAttributes'{..}
          = object
              (catMaybes
                 [Just ("ObjectReference" .= _buoaObjectReference),
                  Just ("AttributeUpdates" .= _buoaAttributeUpdates)])

-- | Represents the output of a @BatchUpdate@ response operation.
--
--
--
-- /See:/ 'batchUpdateObjectAttributesResponse' smart constructor.
newtype BatchUpdateObjectAttributesResponse = BatchUpdateObjectAttributesResponse'
  { _buoaObjectIdentifier :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchUpdateObjectAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'buoaObjectIdentifier' - ID that is associated with the object.
batchUpdateObjectAttributesResponse
    :: BatchUpdateObjectAttributesResponse
batchUpdateObjectAttributesResponse =
  BatchUpdateObjectAttributesResponse' {_buoaObjectIdentifier = Nothing}


-- | ID that is associated with the object.
buoaObjectIdentifier :: Lens' BatchUpdateObjectAttributesResponse (Maybe Text)
buoaObjectIdentifier = lens _buoaObjectIdentifier (\ s a -> s{_buoaObjectIdentifier = a})

instance FromJSON BatchUpdateObjectAttributesResponse
         where
        parseJSON
          = withObject "BatchUpdateObjectAttributesResponse"
              (\ x ->
                 BatchUpdateObjectAttributesResponse' <$>
                   (x .:? "ObjectIdentifier"))

instance Hashable BatchUpdateObjectAttributesResponse
         where

instance NFData BatchUpdateObjectAttributesResponse
         where

-- | Represents the output of a @BatchWrite@ operation.
--
--
--
-- /See:/ 'batchWriteOperation' smart constructor.
data BatchWriteOperation = BatchWriteOperation'
  { _bDeleteObject           :: !(Maybe BatchDeleteObject)
  , _bDetachFromIndex        :: !(Maybe BatchDetachFromIndex)
  , _bRemoveFacetFromObject  :: !(Maybe BatchRemoveFacetFromObject)
  , _bAttachObject           :: !(Maybe BatchAttachObject)
  , _bCreateObject           :: !(Maybe BatchCreateObject)
  , _bAttachTypedLink        :: !(Maybe BatchAttachTypedLink)
  , _bDetachPolicy           :: !(Maybe BatchDetachPolicy)
  , _bCreateIndex            :: !(Maybe BatchCreateIndex)
  , _bDetachObject           :: !(Maybe BatchDetachObject)
  , _bAddFacetToObject       :: !(Maybe BatchAddFacetToObject)
  , _bDetachTypedLink        :: !(Maybe BatchDetachTypedLink)
  , _bUpdateObjectAttributes :: !(Maybe BatchUpdateObjectAttributes)
  , _bAttachPolicy           :: !(Maybe BatchAttachPolicy)
  , _bAttachToIndex          :: !(Maybe BatchAttachToIndex)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchWriteOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bDeleteObject' - Deletes an object in a 'Directory' .
--
-- * 'bDetachFromIndex' - Detaches the specified object from the specified index.
--
-- * 'bRemoveFacetFromObject' - A batch operation that removes a facet from an object.
--
-- * 'bAttachObject' - Attaches an object to a 'Directory' .
--
-- * 'bCreateObject' - Creates an object.
--
-- * 'bAttachTypedLink' - Attaches a typed link to a specified source and target object. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
-- * 'bDetachPolicy' - Detaches a policy from a 'Directory' .
--
-- * 'bCreateIndex' - Creates an index object. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_indexing.html Indexing> for more information.
--
-- * 'bDetachObject' - Detaches an object from a 'Directory' .
--
-- * 'bAddFacetToObject' - A batch operation that adds a facet to an object.
--
-- * 'bDetachTypedLink' - Detaches a typed link from a specified source and target object. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
-- * 'bUpdateObjectAttributes' - Updates a given object's attributes.
--
-- * 'bAttachPolicy' - Attaches a policy object to a regular object. An object can have a limited number of attached policies.
--
-- * 'bAttachToIndex' - Attaches the specified object to the specified index.
batchWriteOperation
    :: BatchWriteOperation
batchWriteOperation =
  BatchWriteOperation'
    { _bDeleteObject = Nothing
    , _bDetachFromIndex = Nothing
    , _bRemoveFacetFromObject = Nothing
    , _bAttachObject = Nothing
    , _bCreateObject = Nothing
    , _bAttachTypedLink = Nothing
    , _bDetachPolicy = Nothing
    , _bCreateIndex = Nothing
    , _bDetachObject = Nothing
    , _bAddFacetToObject = Nothing
    , _bDetachTypedLink = Nothing
    , _bUpdateObjectAttributes = Nothing
    , _bAttachPolicy = Nothing
    , _bAttachToIndex = Nothing
    }


-- | Deletes an object in a 'Directory' .
bDeleteObject :: Lens' BatchWriteOperation (Maybe BatchDeleteObject)
bDeleteObject = lens _bDeleteObject (\ s a -> s{_bDeleteObject = a})

-- | Detaches the specified object from the specified index.
bDetachFromIndex :: Lens' BatchWriteOperation (Maybe BatchDetachFromIndex)
bDetachFromIndex = lens _bDetachFromIndex (\ s a -> s{_bDetachFromIndex = a})

-- | A batch operation that removes a facet from an object.
bRemoveFacetFromObject :: Lens' BatchWriteOperation (Maybe BatchRemoveFacetFromObject)
bRemoveFacetFromObject = lens _bRemoveFacetFromObject (\ s a -> s{_bRemoveFacetFromObject = a})

-- | Attaches an object to a 'Directory' .
bAttachObject :: Lens' BatchWriteOperation (Maybe BatchAttachObject)
bAttachObject = lens _bAttachObject (\ s a -> s{_bAttachObject = a})

-- | Creates an object.
bCreateObject :: Lens' BatchWriteOperation (Maybe BatchCreateObject)
bCreateObject = lens _bCreateObject (\ s a -> s{_bCreateObject = a})

-- | Attaches a typed link to a specified source and target object. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
bAttachTypedLink :: Lens' BatchWriteOperation (Maybe BatchAttachTypedLink)
bAttachTypedLink = lens _bAttachTypedLink (\ s a -> s{_bAttachTypedLink = a})

-- | Detaches a policy from a 'Directory' .
bDetachPolicy :: Lens' BatchWriteOperation (Maybe BatchDetachPolicy)
bDetachPolicy = lens _bDetachPolicy (\ s a -> s{_bDetachPolicy = a})

-- | Creates an index object. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_indexing.html Indexing> for more information.
bCreateIndex :: Lens' BatchWriteOperation (Maybe BatchCreateIndex)
bCreateIndex = lens _bCreateIndex (\ s a -> s{_bCreateIndex = a})

-- | Detaches an object from a 'Directory' .
bDetachObject :: Lens' BatchWriteOperation (Maybe BatchDetachObject)
bDetachObject = lens _bDetachObject (\ s a -> s{_bDetachObject = a})

-- | A batch operation that adds a facet to an object.
bAddFacetToObject :: Lens' BatchWriteOperation (Maybe BatchAddFacetToObject)
bAddFacetToObject = lens _bAddFacetToObject (\ s a -> s{_bAddFacetToObject = a})

-- | Detaches a typed link from a specified source and target object. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
bDetachTypedLink :: Lens' BatchWriteOperation (Maybe BatchDetachTypedLink)
bDetachTypedLink = lens _bDetachTypedLink (\ s a -> s{_bDetachTypedLink = a})

-- | Updates a given object's attributes.
bUpdateObjectAttributes :: Lens' BatchWriteOperation (Maybe BatchUpdateObjectAttributes)
bUpdateObjectAttributes = lens _bUpdateObjectAttributes (\ s a -> s{_bUpdateObjectAttributes = a})

-- | Attaches a policy object to a regular object. An object can have a limited number of attached policies.
bAttachPolicy :: Lens' BatchWriteOperation (Maybe BatchAttachPolicy)
bAttachPolicy = lens _bAttachPolicy (\ s a -> s{_bAttachPolicy = a})

-- | Attaches the specified object to the specified index.
bAttachToIndex :: Lens' BatchWriteOperation (Maybe BatchAttachToIndex)
bAttachToIndex = lens _bAttachToIndex (\ s a -> s{_bAttachToIndex = a})

instance Hashable BatchWriteOperation where

instance NFData BatchWriteOperation where

instance ToJSON BatchWriteOperation where
        toJSON BatchWriteOperation'{..}
          = object
              (catMaybes
                 [("DeleteObject" .=) <$> _bDeleteObject,
                  ("DetachFromIndex" .=) <$> _bDetachFromIndex,
                  ("RemoveFacetFromObject" .=) <$>
                    _bRemoveFacetFromObject,
                  ("AttachObject" .=) <$> _bAttachObject,
                  ("CreateObject" .=) <$> _bCreateObject,
                  ("AttachTypedLink" .=) <$> _bAttachTypedLink,
                  ("DetachPolicy" .=) <$> _bDetachPolicy,
                  ("CreateIndex" .=) <$> _bCreateIndex,
                  ("DetachObject" .=) <$> _bDetachObject,
                  ("AddFacetToObject" .=) <$> _bAddFacetToObject,
                  ("DetachTypedLink" .=) <$> _bDetachTypedLink,
                  ("UpdateObjectAttributes" .=) <$>
                    _bUpdateObjectAttributes,
                  ("AttachPolicy" .=) <$> _bAttachPolicy,
                  ("AttachToIndex" .=) <$> _bAttachToIndex])

-- | Represents the output of a @BatchWrite@ response operation.
--
--
--
-- /See:/ 'batchWriteOperationResponse' smart constructor.
data BatchWriteOperationResponse = BatchWriteOperationResponse'
  { _bwoDeleteObject           :: !(Maybe BatchDeleteObjectResponse)
  , _bwoDetachFromIndex        :: !(Maybe BatchDetachFromIndexResponse)
  , _bwoRemoveFacetFromObject  :: !(Maybe BatchRemoveFacetFromObjectResponse)
  , _bwoAttachObject           :: !(Maybe BatchAttachObjectResponse)
  , _bwoCreateObject           :: !(Maybe BatchCreateObjectResponse)
  , _bwoAttachTypedLink        :: !(Maybe BatchAttachTypedLinkResponse)
  , _bwoDetachPolicy           :: !(Maybe BatchDetachPolicyResponse)
  , _bwoCreateIndex            :: !(Maybe BatchCreateIndexResponse)
  , _bwoDetachObject           :: !(Maybe BatchDetachObjectResponse)
  , _bwoAddFacetToObject       :: !(Maybe BatchAddFacetToObjectResponse)
  , _bwoDetachTypedLink        :: !(Maybe BatchDetachTypedLinkResponse)
  , _bwoUpdateObjectAttributes :: !(Maybe BatchUpdateObjectAttributesResponse)
  , _bwoAttachPolicy           :: !(Maybe BatchAttachPolicyResponse)
  , _bwoAttachToIndex          :: !(Maybe BatchAttachToIndexResponse)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchWriteOperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bwoDeleteObject' - Deletes an object in a 'Directory' .
--
-- * 'bwoDetachFromIndex' - Detaches the specified object from the specified index.
--
-- * 'bwoRemoveFacetFromObject' - The result of a batch remove facet from object operation.
--
-- * 'bwoAttachObject' - Attaches an object to a 'Directory' .
--
-- * 'bwoCreateObject' - Creates an object in a 'Directory' .
--
-- * 'bwoAttachTypedLink' - Attaches a typed link to a specified source and target object. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
-- * 'bwoDetachPolicy' - Detaches a policy from a 'Directory' .
--
-- * 'bwoCreateIndex' - Creates an index object. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_indexing.html Indexing> for more information.
--
-- * 'bwoDetachObject' - Detaches an object from a 'Directory' .
--
-- * 'bwoAddFacetToObject' - The result of an add facet to object batch operation.
--
-- * 'bwoDetachTypedLink' - Detaches a typed link from a specified source and target object. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
--
-- * 'bwoUpdateObjectAttributes' - Updates a given object’s attributes.
--
-- * 'bwoAttachPolicy' - Attaches a policy object to a regular object. An object can have a limited number of attached policies.
--
-- * 'bwoAttachToIndex' - Attaches the specified object to the specified index.
batchWriteOperationResponse
    :: BatchWriteOperationResponse
batchWriteOperationResponse =
  BatchWriteOperationResponse'
    { _bwoDeleteObject = Nothing
    , _bwoDetachFromIndex = Nothing
    , _bwoRemoveFacetFromObject = Nothing
    , _bwoAttachObject = Nothing
    , _bwoCreateObject = Nothing
    , _bwoAttachTypedLink = Nothing
    , _bwoDetachPolicy = Nothing
    , _bwoCreateIndex = Nothing
    , _bwoDetachObject = Nothing
    , _bwoAddFacetToObject = Nothing
    , _bwoDetachTypedLink = Nothing
    , _bwoUpdateObjectAttributes = Nothing
    , _bwoAttachPolicy = Nothing
    , _bwoAttachToIndex = Nothing
    }


-- | Deletes an object in a 'Directory' .
bwoDeleteObject :: Lens' BatchWriteOperationResponse (Maybe BatchDeleteObjectResponse)
bwoDeleteObject = lens _bwoDeleteObject (\ s a -> s{_bwoDeleteObject = a})

-- | Detaches the specified object from the specified index.
bwoDetachFromIndex :: Lens' BatchWriteOperationResponse (Maybe BatchDetachFromIndexResponse)
bwoDetachFromIndex = lens _bwoDetachFromIndex (\ s a -> s{_bwoDetachFromIndex = a})

-- | The result of a batch remove facet from object operation.
bwoRemoveFacetFromObject :: Lens' BatchWriteOperationResponse (Maybe BatchRemoveFacetFromObjectResponse)
bwoRemoveFacetFromObject = lens _bwoRemoveFacetFromObject (\ s a -> s{_bwoRemoveFacetFromObject = a})

-- | Attaches an object to a 'Directory' .
bwoAttachObject :: Lens' BatchWriteOperationResponse (Maybe BatchAttachObjectResponse)
bwoAttachObject = lens _bwoAttachObject (\ s a -> s{_bwoAttachObject = a})

-- | Creates an object in a 'Directory' .
bwoCreateObject :: Lens' BatchWriteOperationResponse (Maybe BatchCreateObjectResponse)
bwoCreateObject = lens _bwoCreateObject (\ s a -> s{_bwoCreateObject = a})

-- | Attaches a typed link to a specified source and target object. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
bwoAttachTypedLink :: Lens' BatchWriteOperationResponse (Maybe BatchAttachTypedLinkResponse)
bwoAttachTypedLink = lens _bwoAttachTypedLink (\ s a -> s{_bwoAttachTypedLink = a})

-- | Detaches a policy from a 'Directory' .
bwoDetachPolicy :: Lens' BatchWriteOperationResponse (Maybe BatchDetachPolicyResponse)
bwoDetachPolicy = lens _bwoDetachPolicy (\ s a -> s{_bwoDetachPolicy = a})

-- | Creates an index object. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_indexing.html Indexing> for more information.
bwoCreateIndex :: Lens' BatchWriteOperationResponse (Maybe BatchCreateIndexResponse)
bwoCreateIndex = lens _bwoCreateIndex (\ s a -> s{_bwoCreateIndex = a})

-- | Detaches an object from a 'Directory' .
bwoDetachObject :: Lens' BatchWriteOperationResponse (Maybe BatchDetachObjectResponse)
bwoDetachObject = lens _bwoDetachObject (\ s a -> s{_bwoDetachObject = a})

-- | The result of an add facet to object batch operation.
bwoAddFacetToObject :: Lens' BatchWriteOperationResponse (Maybe BatchAddFacetToObjectResponse)
bwoAddFacetToObject = lens _bwoAddFacetToObject (\ s a -> s{_bwoAddFacetToObject = a})

-- | Detaches a typed link from a specified source and target object. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#typedlink Typed link> .
bwoDetachTypedLink :: Lens' BatchWriteOperationResponse (Maybe BatchDetachTypedLinkResponse)
bwoDetachTypedLink = lens _bwoDetachTypedLink (\ s a -> s{_bwoDetachTypedLink = a})

-- | Updates a given object’s attributes.
bwoUpdateObjectAttributes :: Lens' BatchWriteOperationResponse (Maybe BatchUpdateObjectAttributesResponse)
bwoUpdateObjectAttributes = lens _bwoUpdateObjectAttributes (\ s a -> s{_bwoUpdateObjectAttributes = a})

-- | Attaches a policy object to a regular object. An object can have a limited number of attached policies.
bwoAttachPolicy :: Lens' BatchWriteOperationResponse (Maybe BatchAttachPolicyResponse)
bwoAttachPolicy = lens _bwoAttachPolicy (\ s a -> s{_bwoAttachPolicy = a})

-- | Attaches the specified object to the specified index.
bwoAttachToIndex :: Lens' BatchWriteOperationResponse (Maybe BatchAttachToIndexResponse)
bwoAttachToIndex = lens _bwoAttachToIndex (\ s a -> s{_bwoAttachToIndex = a})

instance FromJSON BatchWriteOperationResponse where
        parseJSON
          = withObject "BatchWriteOperationResponse"
              (\ x ->
                 BatchWriteOperationResponse' <$>
                   (x .:? "DeleteObject") <*> (x .:? "DetachFromIndex")
                     <*> (x .:? "RemoveFacetFromObject")
                     <*> (x .:? "AttachObject")
                     <*> (x .:? "CreateObject")
                     <*> (x .:? "AttachTypedLink")
                     <*> (x .:? "DetachPolicy")
                     <*> (x .:? "CreateIndex")
                     <*> (x .:? "DetachObject")
                     <*> (x .:? "AddFacetToObject")
                     <*> (x .:? "DetachTypedLink")
                     <*> (x .:? "UpdateObjectAttributes")
                     <*> (x .:? "AttachPolicy")
                     <*> (x .:? "AttachToIndex"))

instance Hashable BatchWriteOperationResponse where

instance NFData BatchWriteOperationResponse where

-- | Directory structure that includes the directory name and directory ARN.
--
--
--
-- /See:/ 'directory' smart constructor.
data Directory = Directory'
  { _dDirectoryARN     :: !(Maybe Text)
  , _dState            :: !(Maybe DirectoryState)
  , _dName             :: !(Maybe Text)
  , _dCreationDateTime :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Directory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the directory. For more information, see 'arns' .
--
-- * 'dState' - The state of the directory. Can be either @Enabled@ , @Disabled@ , or @Deleted@ .
--
-- * 'dName' - The name of the directory.
--
-- * 'dCreationDateTime' - The date and time when the directory was created.
directory
    :: Directory
directory =
  Directory'
    { _dDirectoryARN = Nothing
    , _dState = Nothing
    , _dName = Nothing
    , _dCreationDateTime = Nothing
    }


-- | The Amazon Resource Name (ARN) that is associated with the directory. For more information, see 'arns' .
dDirectoryARN :: Lens' Directory (Maybe Text)
dDirectoryARN = lens _dDirectoryARN (\ s a -> s{_dDirectoryARN = a})

-- | The state of the directory. Can be either @Enabled@ , @Disabled@ , or @Deleted@ .
dState :: Lens' Directory (Maybe DirectoryState)
dState = lens _dState (\ s a -> s{_dState = a})

-- | The name of the directory.
dName :: Lens' Directory (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a})

-- | The date and time when the directory was created.
dCreationDateTime :: Lens' Directory (Maybe UTCTime)
dCreationDateTime = lens _dCreationDateTime (\ s a -> s{_dCreationDateTime = a}) . mapping _Time

instance FromJSON Directory where
        parseJSON
          = withObject "Directory"
              (\ x ->
                 Directory' <$>
                   (x .:? "DirectoryArn") <*> (x .:? "State") <*>
                     (x .:? "Name")
                     <*> (x .:? "CreationDateTime"))

instance Hashable Directory where

instance NFData Directory where

-- | A structure that contains @Name@ , @ARN@ , @Attributes@ , @'Rule' s@ , and @ObjectTypes@ . See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/whatarefacets.html Facets> for more information.
--
--
--
-- /See:/ 'facet' smart constructor.
data Facet = Facet'
  { _fObjectType :: !(Maybe ObjectType)
  , _fName       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Facet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fObjectType' - The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
--
-- * 'fName' - The name of the 'Facet' .
facet
    :: Facet
facet = Facet' {_fObjectType = Nothing, _fName = Nothing}


-- | The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
fObjectType :: Lens' Facet (Maybe ObjectType)
fObjectType = lens _fObjectType (\ s a -> s{_fObjectType = a})

-- | The name of the 'Facet' .
fName :: Lens' Facet (Maybe Text)
fName = lens _fName (\ s a -> s{_fName = a})

instance FromJSON Facet where
        parseJSON
          = withObject "Facet"
              (\ x ->
                 Facet' <$> (x .:? "ObjectType") <*> (x .:? "Name"))

instance Hashable Facet where

instance NFData Facet where

-- | An attribute that is associated with the 'Facet' .
--
--
--
-- /See:/ 'facetAttribute' smart constructor.
data FacetAttribute = FacetAttribute'
  { _faAttributeReference  :: !(Maybe FacetAttributeReference)
  , _faAttributeDefinition :: !(Maybe FacetAttributeDefinition)
  , _faRequiredBehavior    :: !(Maybe RequiredAttributeBehavior)
  , _faName                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FacetAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faAttributeReference' - An attribute reference that is associated with the attribute. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
--
-- * 'faAttributeDefinition' - A facet attribute consists of either a definition or a reference. This structure contains the attribute definition. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
--
-- * 'faRequiredBehavior' - The required behavior of the @FacetAttribute@ .
--
-- * 'faName' - The name of the facet attribute.
facetAttribute
    :: Text -- ^ 'faName'
    -> FacetAttribute
facetAttribute pName_ =
  FacetAttribute'
    { _faAttributeReference = Nothing
    , _faAttributeDefinition = Nothing
    , _faRequiredBehavior = Nothing
    , _faName = pName_
    }


-- | An attribute reference that is associated with the attribute. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
faAttributeReference :: Lens' FacetAttribute (Maybe FacetAttributeReference)
faAttributeReference = lens _faAttributeReference (\ s a -> s{_faAttributeReference = a})

-- | A facet attribute consists of either a definition or a reference. This structure contains the attribute definition. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
faAttributeDefinition :: Lens' FacetAttribute (Maybe FacetAttributeDefinition)
faAttributeDefinition = lens _faAttributeDefinition (\ s a -> s{_faAttributeDefinition = a})

-- | The required behavior of the @FacetAttribute@ .
faRequiredBehavior :: Lens' FacetAttribute (Maybe RequiredAttributeBehavior)
faRequiredBehavior = lens _faRequiredBehavior (\ s a -> s{_faRequiredBehavior = a})

-- | The name of the facet attribute.
faName :: Lens' FacetAttribute Text
faName = lens _faName (\ s a -> s{_faName = a})

instance FromJSON FacetAttribute where
        parseJSON
          = withObject "FacetAttribute"
              (\ x ->
                 FacetAttribute' <$>
                   (x .:? "AttributeReference") <*>
                     (x .:? "AttributeDefinition")
                     <*> (x .:? "RequiredBehavior")
                     <*> (x .: "Name"))

instance Hashable FacetAttribute where

instance NFData FacetAttribute where

instance ToJSON FacetAttribute where
        toJSON FacetAttribute'{..}
          = object
              (catMaybes
                 [("AttributeReference" .=) <$> _faAttributeReference,
                  ("AttributeDefinition" .=) <$>
                    _faAttributeDefinition,
                  ("RequiredBehavior" .=) <$> _faRequiredBehavior,
                  Just ("Name" .= _faName)])

-- | A facet attribute definition. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
--
--
--
-- /See:/ 'facetAttributeDefinition' smart constructor.
data FacetAttributeDefinition = FacetAttributeDefinition'
  { _fadRules        :: !(Maybe (Map Text Rule))
  , _fadDefaultValue :: !(Maybe TypedAttributeValue)
  , _fadIsImmutable  :: !(Maybe Bool)
  , _fadType         :: !FacetAttributeType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FacetAttributeDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fadRules' - Validation rules attached to the attribute definition.
--
-- * 'fadDefaultValue' - The default value of the attribute (if configured).
--
-- * 'fadIsImmutable' - Whether the attribute is mutable or not.
--
-- * 'fadType' - The type of the attribute.
facetAttributeDefinition
    :: FacetAttributeType -- ^ 'fadType'
    -> FacetAttributeDefinition
facetAttributeDefinition pType_ =
  FacetAttributeDefinition'
    { _fadRules = Nothing
    , _fadDefaultValue = Nothing
    , _fadIsImmutable = Nothing
    , _fadType = pType_
    }


-- | Validation rules attached to the attribute definition.
fadRules :: Lens' FacetAttributeDefinition (HashMap Text Rule)
fadRules = lens _fadRules (\ s a -> s{_fadRules = a}) . _Default . _Map

-- | The default value of the attribute (if configured).
fadDefaultValue :: Lens' FacetAttributeDefinition (Maybe TypedAttributeValue)
fadDefaultValue = lens _fadDefaultValue (\ s a -> s{_fadDefaultValue = a})

-- | Whether the attribute is mutable or not.
fadIsImmutable :: Lens' FacetAttributeDefinition (Maybe Bool)
fadIsImmutable = lens _fadIsImmutable (\ s a -> s{_fadIsImmutable = a})

-- | The type of the attribute.
fadType :: Lens' FacetAttributeDefinition FacetAttributeType
fadType = lens _fadType (\ s a -> s{_fadType = a})

instance FromJSON FacetAttributeDefinition where
        parseJSON
          = withObject "FacetAttributeDefinition"
              (\ x ->
                 FacetAttributeDefinition' <$>
                   (x .:? "Rules" .!= mempty) <*> (x .:? "DefaultValue")
                     <*> (x .:? "IsImmutable")
                     <*> (x .: "Type"))

instance Hashable FacetAttributeDefinition where

instance NFData FacetAttributeDefinition where

instance ToJSON FacetAttributeDefinition where
        toJSON FacetAttributeDefinition'{..}
          = object
              (catMaybes
                 [("Rules" .=) <$> _fadRules,
                  ("DefaultValue" .=) <$> _fadDefaultValue,
                  ("IsImmutable" .=) <$> _fadIsImmutable,
                  Just ("Type" .= _fadType)])

-- | The facet attribute reference that specifies the attribute definition that contains the attribute facet name and attribute name.
--
--
--
-- /See:/ 'facetAttributeReference' smart constructor.
data FacetAttributeReference = FacetAttributeReference'
  { _farTargetFacetName     :: !Text
  , _farTargetAttributeName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FacetAttributeReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'farTargetFacetName' - The target facet name that is associated with the facet reference. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
--
-- * 'farTargetAttributeName' - The target attribute name that is associated with the facet reference. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
facetAttributeReference
    :: Text -- ^ 'farTargetFacetName'
    -> Text -- ^ 'farTargetAttributeName'
    -> FacetAttributeReference
facetAttributeReference pTargetFacetName_ pTargetAttributeName_ =
  FacetAttributeReference'
    { _farTargetFacetName = pTargetFacetName_
    , _farTargetAttributeName = pTargetAttributeName_
    }


-- | The target facet name that is associated with the facet reference. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
farTargetFacetName :: Lens' FacetAttributeReference Text
farTargetFacetName = lens _farTargetFacetName (\ s a -> s{_farTargetFacetName = a})

-- | The target attribute name that is associated with the facet reference. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
farTargetAttributeName :: Lens' FacetAttributeReference Text
farTargetAttributeName = lens _farTargetAttributeName (\ s a -> s{_farTargetAttributeName = a})

instance FromJSON FacetAttributeReference where
        parseJSON
          = withObject "FacetAttributeReference"
              (\ x ->
                 FacetAttributeReference' <$>
                   (x .: "TargetFacetName") <*>
                     (x .: "TargetAttributeName"))

instance Hashable FacetAttributeReference where

instance NFData FacetAttributeReference where

instance ToJSON FacetAttributeReference where
        toJSON FacetAttributeReference'{..}
          = object
              (catMaybes
                 [Just ("TargetFacetName" .= _farTargetFacetName),
                  Just
                    ("TargetAttributeName" .= _farTargetAttributeName)])

-- | A structure that contains information used to update an attribute.
--
--
--
-- /See:/ 'facetAttributeUpdate' smart constructor.
data FacetAttributeUpdate = FacetAttributeUpdate'
  { _fauAttribute :: !(Maybe FacetAttribute)
  , _fauAction    :: !(Maybe UpdateActionType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'FacetAttributeUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fauAttribute' - The attribute to update.
--
-- * 'fauAction' - The action to perform when updating the attribute.
facetAttributeUpdate
    :: FacetAttributeUpdate
facetAttributeUpdate =
  FacetAttributeUpdate' {_fauAttribute = Nothing, _fauAction = Nothing}


-- | The attribute to update.
fauAttribute :: Lens' FacetAttributeUpdate (Maybe FacetAttribute)
fauAttribute = lens _fauAttribute (\ s a -> s{_fauAttribute = a})

-- | The action to perform when updating the attribute.
fauAction :: Lens' FacetAttributeUpdate (Maybe UpdateActionType)
fauAction = lens _fauAction (\ s a -> s{_fauAction = a})

instance Hashable FacetAttributeUpdate where

instance NFData FacetAttributeUpdate where

instance ToJSON FacetAttributeUpdate where
        toJSON FacetAttributeUpdate'{..}
          = object
              (catMaybes
                 [("Attribute" .=) <$> _fauAttribute,
                  ("Action" .=) <$> _fauAction])

-- | Represents an index and an attached object.
--
--
--
-- /See:/ 'indexAttachment' smart constructor.
data IndexAttachment = IndexAttachment'
  { _iaIndexedAttributes :: !(Maybe [AttributeKeyAndValue])
  , _iaObjectIdentifier  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IndexAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaIndexedAttributes' - The indexed attribute values.
--
-- * 'iaObjectIdentifier' - In response to 'ListIndex' , the @ObjectIdentifier@ of the object attached to the index. In response to 'ListAttachedIndices' , the @ObjectIdentifier@ of the index attached to the object. This field will always contain the @ObjectIdentifier@ of the object on the opposite side of the attachment specified in the query.
indexAttachment
    :: IndexAttachment
indexAttachment =
  IndexAttachment'
    {_iaIndexedAttributes = Nothing, _iaObjectIdentifier = Nothing}


-- | The indexed attribute values.
iaIndexedAttributes :: Lens' IndexAttachment [AttributeKeyAndValue]
iaIndexedAttributes = lens _iaIndexedAttributes (\ s a -> s{_iaIndexedAttributes = a}) . _Default . _Coerce

-- | In response to 'ListIndex' , the @ObjectIdentifier@ of the object attached to the index. In response to 'ListAttachedIndices' , the @ObjectIdentifier@ of the index attached to the object. This field will always contain the @ObjectIdentifier@ of the object on the opposite side of the attachment specified in the query.
iaObjectIdentifier :: Lens' IndexAttachment (Maybe Text)
iaObjectIdentifier = lens _iaObjectIdentifier (\ s a -> s{_iaObjectIdentifier = a})

instance FromJSON IndexAttachment where
        parseJSON
          = withObject "IndexAttachment"
              (\ x ->
                 IndexAttachment' <$>
                   (x .:? "IndexedAttributes" .!= mempty) <*>
                     (x .:? "ObjectIdentifier"))

instance Hashable IndexAttachment where

instance NFData IndexAttachment where

-- | The action to take on the object attribute.
--
--
--
-- /See:/ 'objectAttributeAction' smart constructor.
data ObjectAttributeAction = ObjectAttributeAction'
  { _oaaObjectAttributeActionType  :: !(Maybe UpdateActionType)
  , _oaaObjectAttributeUpdateValue :: !(Maybe TypedAttributeValue)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectAttributeAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oaaObjectAttributeActionType' - A type that can be either @Update@ or @Delete@ .
--
-- * 'oaaObjectAttributeUpdateValue' - The value that you want to update to.
objectAttributeAction
    :: ObjectAttributeAction
objectAttributeAction =
  ObjectAttributeAction'
    { _oaaObjectAttributeActionType = Nothing
    , _oaaObjectAttributeUpdateValue = Nothing
    }


-- | A type that can be either @Update@ or @Delete@ .
oaaObjectAttributeActionType :: Lens' ObjectAttributeAction (Maybe UpdateActionType)
oaaObjectAttributeActionType = lens _oaaObjectAttributeActionType (\ s a -> s{_oaaObjectAttributeActionType = a})

-- | The value that you want to update to.
oaaObjectAttributeUpdateValue :: Lens' ObjectAttributeAction (Maybe TypedAttributeValue)
oaaObjectAttributeUpdateValue = lens _oaaObjectAttributeUpdateValue (\ s a -> s{_oaaObjectAttributeUpdateValue = a})

instance Hashable ObjectAttributeAction where

instance NFData ObjectAttributeAction where

instance ToJSON ObjectAttributeAction where
        toJSON ObjectAttributeAction'{..}
          = object
              (catMaybes
                 [("ObjectAttributeActionType" .=) <$>
                    _oaaObjectAttributeActionType,
                  ("ObjectAttributeUpdateValue" .=) <$>
                    _oaaObjectAttributeUpdateValue])

-- | A range of attributes.
--
--
--
-- /See:/ 'objectAttributeRange' smart constructor.
data ObjectAttributeRange = ObjectAttributeRange'
  { _oarRange        :: !(Maybe TypedAttributeValueRange)
  , _oarAttributeKey :: !(Maybe AttributeKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectAttributeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oarRange' - The range of attribute values being selected.
--
-- * 'oarAttributeKey' - The key of the attribute that the attribute range covers.
objectAttributeRange
    :: ObjectAttributeRange
objectAttributeRange =
  ObjectAttributeRange' {_oarRange = Nothing, _oarAttributeKey = Nothing}


-- | The range of attribute values being selected.
oarRange :: Lens' ObjectAttributeRange (Maybe TypedAttributeValueRange)
oarRange = lens _oarRange (\ s a -> s{_oarRange = a})

-- | The key of the attribute that the attribute range covers.
oarAttributeKey :: Lens' ObjectAttributeRange (Maybe AttributeKey)
oarAttributeKey = lens _oarAttributeKey (\ s a -> s{_oarAttributeKey = a})

instance Hashable ObjectAttributeRange where

instance NFData ObjectAttributeRange where

instance ToJSON ObjectAttributeRange where
        toJSON ObjectAttributeRange'{..}
          = object
              (catMaybes
                 [("Range" .=) <$> _oarRange,
                  ("AttributeKey" .=) <$> _oarAttributeKey])

-- | Structure that contains attribute update information.
--
--
--
-- /See:/ 'objectAttributeUpdate' smart constructor.
data ObjectAttributeUpdate = ObjectAttributeUpdate'
  { _oauObjectAttributeAction :: !(Maybe ObjectAttributeAction)
  , _oauObjectAttributeKey    :: !(Maybe AttributeKey)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectAttributeUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oauObjectAttributeAction' - The action to perform as part of the attribute update.
--
-- * 'oauObjectAttributeKey' - The key of the attribute being updated.
objectAttributeUpdate
    :: ObjectAttributeUpdate
objectAttributeUpdate =
  ObjectAttributeUpdate'
    {_oauObjectAttributeAction = Nothing, _oauObjectAttributeKey = Nothing}


-- | The action to perform as part of the attribute update.
oauObjectAttributeAction :: Lens' ObjectAttributeUpdate (Maybe ObjectAttributeAction)
oauObjectAttributeAction = lens _oauObjectAttributeAction (\ s a -> s{_oauObjectAttributeAction = a})

-- | The key of the attribute being updated.
oauObjectAttributeKey :: Lens' ObjectAttributeUpdate (Maybe AttributeKey)
oauObjectAttributeKey = lens _oauObjectAttributeKey (\ s a -> s{_oauObjectAttributeKey = a})

instance Hashable ObjectAttributeUpdate where

instance NFData ObjectAttributeUpdate where

instance ToJSON ObjectAttributeUpdate where
        toJSON ObjectAttributeUpdate'{..}
          = object
              (catMaybes
                 [("ObjectAttributeAction" .=) <$>
                    _oauObjectAttributeAction,
                  ("ObjectAttributeKey" .=) <$>
                    _oauObjectAttributeKey])

-- | The reference that identifies an object.
--
--
--
-- /See:/ 'objectReference' smart constructor.
newtype ObjectReference = ObjectReference'
  { _orSelector :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ObjectReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orSelector' - A path selector supports easy selection of an object by the parent/child links leading to it from the directory root. Use the link names from each parent/child link to construct the path. Path selectors start with a slash (/) and link names are separated by slashes. For more information about paths, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#accessingobjects Accessing Objects> . You can identify an object in one of the following ways:     * /> ObjectIdentifier/ - An object identifier is an opaque string provided by Amazon Cloud Directory. When creating objects, the system will provide you with the identifier of the created object. An object’s identifier is immutable and no two objects will ever share the same object identifier     * /\/some\/path/ - Identifies the object based on path     * /#SomeBatchReference/ - Identifies the object in a batch call
objectReference
    :: ObjectReference
objectReference = ObjectReference' {_orSelector = Nothing}


-- | A path selector supports easy selection of an object by the parent/child links leading to it from the directory root. Use the link names from each parent/child link to construct the path. Path selectors start with a slash (/) and link names are separated by slashes. For more information about paths, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#accessingobjects Accessing Objects> . You can identify an object in one of the following ways:     * /> ObjectIdentifier/ - An object identifier is an opaque string provided by Amazon Cloud Directory. When creating objects, the system will provide you with the identifier of the created object. An object’s identifier is immutable and no two objects will ever share the same object identifier     * /\/some\/path/ - Identifies the object based on path     * /#SomeBatchReference/ - Identifies the object in a batch call
orSelector :: Lens' ObjectReference (Maybe Text)
orSelector = lens _orSelector (\ s a -> s{_orSelector = a})

instance FromJSON ObjectReference where
        parseJSON
          = withObject "ObjectReference"
              (\ x -> ObjectReference' <$> (x .:? "Selector"))

instance Hashable ObjectReference where

instance NFData ObjectReference where

instance ToJSON ObjectReference where
        toJSON ObjectReference'{..}
          = object
              (catMaybes [("Selector" .=) <$> _orSelector])

-- | Returns the path to the @ObjectIdentifiers@ that is associated with the directory.
--
--
--
-- /See:/ 'pathToObjectIdentifiers' smart constructor.
data PathToObjectIdentifiers = PathToObjectIdentifiers'
  { _ptoiObjectIdentifiers :: !(Maybe [Text])
  , _ptoiPath              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PathToObjectIdentifiers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptoiObjectIdentifiers' - Lists @ObjectIdentifiers@ starting from directory root to the object in the request.
--
-- * 'ptoiPath' - The path that is used to identify the object starting from directory root.
pathToObjectIdentifiers
    :: PathToObjectIdentifiers
pathToObjectIdentifiers =
  PathToObjectIdentifiers'
    {_ptoiObjectIdentifiers = Nothing, _ptoiPath = Nothing}


-- | Lists @ObjectIdentifiers@ starting from directory root to the object in the request.
ptoiObjectIdentifiers :: Lens' PathToObjectIdentifiers [Text]
ptoiObjectIdentifiers = lens _ptoiObjectIdentifiers (\ s a -> s{_ptoiObjectIdentifiers = a}) . _Default . _Coerce

-- | The path that is used to identify the object starting from directory root.
ptoiPath :: Lens' PathToObjectIdentifiers (Maybe Text)
ptoiPath = lens _ptoiPath (\ s a -> s{_ptoiPath = a})

instance FromJSON PathToObjectIdentifiers where
        parseJSON
          = withObject "PathToObjectIdentifiers"
              (\ x ->
                 PathToObjectIdentifiers' <$>
                   (x .:? "ObjectIdentifiers" .!= mempty) <*>
                     (x .:? "Path"))

instance Hashable PathToObjectIdentifiers where

instance NFData PathToObjectIdentifiers where

-- | Contains the @PolicyType@ , @PolicyId@ , and the @ObjectIdentifier@ to which it is attached. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .
--
--
--
-- /See:/ 'policyAttachment' smart constructor.
data PolicyAttachment = PolicyAttachment'
  { _paPolicyId         :: !(Maybe Text)
  , _paPolicyType       :: !(Maybe Text)
  , _paObjectIdentifier :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paPolicyId' - The ID of @PolicyAttachment@ .
--
-- * 'paPolicyType' - The type of policy that can be associated with @PolicyAttachment@ .
--
-- * 'paObjectIdentifier' - The @ObjectIdentifier@ that is associated with @PolicyAttachment@ .
policyAttachment
    :: PolicyAttachment
policyAttachment =
  PolicyAttachment'
    { _paPolicyId = Nothing
    , _paPolicyType = Nothing
    , _paObjectIdentifier = Nothing
    }


-- | The ID of @PolicyAttachment@ .
paPolicyId :: Lens' PolicyAttachment (Maybe Text)
paPolicyId = lens _paPolicyId (\ s a -> s{_paPolicyId = a})

-- | The type of policy that can be associated with @PolicyAttachment@ .
paPolicyType :: Lens' PolicyAttachment (Maybe Text)
paPolicyType = lens _paPolicyType (\ s a -> s{_paPolicyType = a})

-- | The @ObjectIdentifier@ that is associated with @PolicyAttachment@ .
paObjectIdentifier :: Lens' PolicyAttachment (Maybe Text)
paObjectIdentifier = lens _paObjectIdentifier (\ s a -> s{_paObjectIdentifier = a})

instance FromJSON PolicyAttachment where
        parseJSON
          = withObject "PolicyAttachment"
              (\ x ->
                 PolicyAttachment' <$>
                   (x .:? "PolicyId") <*> (x .:? "PolicyType") <*>
                     (x .:? "ObjectIdentifier"))

instance Hashable PolicyAttachment where

instance NFData PolicyAttachment where

-- | Used when a regular object exists in a 'Directory' and you want to find all of the policies that are associated with that object and the parent to that object.
--
--
--
-- /See:/ 'policyToPath' smart constructor.
data PolicyToPath = PolicyToPath'
  { _ptpPath     :: !(Maybe Text)
  , _ptpPolicies :: !(Maybe [PolicyAttachment])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PolicyToPath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptpPath' - The path that is referenced from the root.
--
-- * 'ptpPolicies' - List of policy objects.
policyToPath
    :: PolicyToPath
policyToPath = PolicyToPath' {_ptpPath = Nothing, _ptpPolicies = Nothing}


-- | The path that is referenced from the root.
ptpPath :: Lens' PolicyToPath (Maybe Text)
ptpPath = lens _ptpPath (\ s a -> s{_ptpPath = a})

-- | List of policy objects.
ptpPolicies :: Lens' PolicyToPath [PolicyAttachment]
ptpPolicies = lens _ptpPolicies (\ s a -> s{_ptpPolicies = a}) . _Default . _Coerce

instance FromJSON PolicyToPath where
        parseJSON
          = withObject "PolicyToPath"
              (\ x ->
                 PolicyToPath' <$>
                   (x .:? "Path") <*> (x .:? "Policies" .!= mempty))

instance Hashable PolicyToPath where

instance NFData PolicyToPath where

-- | Contains an Amazon Resource Name (ARN) and parameters that are associated with the rule.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
  { _rParameters :: !(Maybe (Map Text Text))
  , _rType       :: !(Maybe RuleType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rParameters' - The minimum and maximum parameters that are associated with the rule.
--
-- * 'rType' - The type of attribute validation rule.
rule
    :: Rule
rule = Rule' {_rParameters = Nothing, _rType = Nothing}


-- | The minimum and maximum parameters that are associated with the rule.
rParameters :: Lens' Rule (HashMap Text Text)
rParameters = lens _rParameters (\ s a -> s{_rParameters = a}) . _Default . _Map

-- | The type of attribute validation rule.
rType :: Lens' Rule (Maybe RuleType)
rType = lens _rType (\ s a -> s{_rType = a})

instance FromJSON Rule where
        parseJSON
          = withObject "Rule"
              (\ x ->
                 Rule' <$>
                   (x .:? "Parameters" .!= mempty) <*> (x .:? "Type"))

instance Hashable Rule where

instance NFData Rule where

instance ToJSON Rule where
        toJSON Rule'{..}
          = object
              (catMaybes
                 [("Parameters" .=) <$> _rParameters,
                  ("Type" .=) <$> _rType])

-- | A facet.
--
--
--
-- /See:/ 'schemaFacet' smart constructor.
data SchemaFacet = SchemaFacet'
  { _sfFacetName :: !(Maybe Text)
  , _sfSchemaARN :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SchemaFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfFacetName' - The name of the facet.
--
-- * 'sfSchemaARN' - The ARN of the schema that contains the facet with no minor component. See 'arns' and <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/inplaceschemaupgrade.html In-Place Schema Upgrade> for a description of when to provide minor versions.
schemaFacet
    :: SchemaFacet
schemaFacet = SchemaFacet' {_sfFacetName = Nothing, _sfSchemaARN = Nothing}


-- | The name of the facet.
sfFacetName :: Lens' SchemaFacet (Maybe Text)
sfFacetName = lens _sfFacetName (\ s a -> s{_sfFacetName = a})

-- | The ARN of the schema that contains the facet with no minor component. See 'arns' and <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/inplaceschemaupgrade.html In-Place Schema Upgrade> for a description of when to provide minor versions.
sfSchemaARN :: Lens' SchemaFacet (Maybe Text)
sfSchemaARN = lens _sfSchemaARN (\ s a -> s{_sfSchemaARN = a})

instance FromJSON SchemaFacet where
        parseJSON
          = withObject "SchemaFacet"
              (\ x ->
                 SchemaFacet' <$>
                   (x .:? "FacetName") <*> (x .:? "SchemaArn"))

instance Hashable SchemaFacet where

instance NFData SchemaFacet where

instance ToJSON SchemaFacet where
        toJSON SchemaFacet'{..}
          = object
              (catMaybes
                 [("FacetName" .=) <$> _sfFacetName,
                  ("SchemaArn" .=) <$> _sfSchemaARN])

-- | The tag structure that contains a tag key and value.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value that is associated with the tag.
--
-- * 'tagKey' - The key that is associated with the tag.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The value that is associated with the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key that is associated with the tag.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])

-- | Represents the data for a typed attribute. You can set one, and only one, of the elements. Each attribute in an item is a name-value pair. Attributes have a single value.
--
--
--
-- /See:/ 'typedAttributeValue' smart constructor.
data TypedAttributeValue = TypedAttributeValue'
  { _tavBinaryValue   :: !(Maybe Base64)
  , _tavDatetimeValue :: !(Maybe POSIX)
  , _tavNumberValue   :: !(Maybe Text)
  , _tavStringValue   :: !(Maybe Text)
  , _tavBooleanValue  :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TypedAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tavBinaryValue' - A binary data value.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
--
-- * 'tavDatetimeValue' - A date and time value.
--
-- * 'tavNumberValue' - A number data value.
--
-- * 'tavStringValue' - A string data value.
--
-- * 'tavBooleanValue' - A Boolean data value.
typedAttributeValue
    :: TypedAttributeValue
typedAttributeValue =
  TypedAttributeValue'
    { _tavBinaryValue = Nothing
    , _tavDatetimeValue = Nothing
    , _tavNumberValue = Nothing
    , _tavStringValue = Nothing
    , _tavBooleanValue = Nothing
    }


-- | A binary data value.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
tavBinaryValue :: Lens' TypedAttributeValue (Maybe ByteString)
tavBinaryValue = lens _tavBinaryValue (\ s a -> s{_tavBinaryValue = a}) . mapping _Base64

-- | A date and time value.
tavDatetimeValue :: Lens' TypedAttributeValue (Maybe UTCTime)
tavDatetimeValue = lens _tavDatetimeValue (\ s a -> s{_tavDatetimeValue = a}) . mapping _Time

-- | A number data value.
tavNumberValue :: Lens' TypedAttributeValue (Maybe Text)
tavNumberValue = lens _tavNumberValue (\ s a -> s{_tavNumberValue = a})

-- | A string data value.
tavStringValue :: Lens' TypedAttributeValue (Maybe Text)
tavStringValue = lens _tavStringValue (\ s a -> s{_tavStringValue = a})

-- | A Boolean data value.
tavBooleanValue :: Lens' TypedAttributeValue (Maybe Bool)
tavBooleanValue = lens _tavBooleanValue (\ s a -> s{_tavBooleanValue = a})

instance FromJSON TypedAttributeValue where
        parseJSON
          = withObject "TypedAttributeValue"
              (\ x ->
                 TypedAttributeValue' <$>
                   (x .:? "BinaryValue") <*> (x .:? "DatetimeValue") <*>
                     (x .:? "NumberValue")
                     <*> (x .:? "StringValue")
                     <*> (x .:? "BooleanValue"))

instance Hashable TypedAttributeValue where

instance NFData TypedAttributeValue where

instance ToJSON TypedAttributeValue where
        toJSON TypedAttributeValue'{..}
          = object
              (catMaybes
                 [("BinaryValue" .=) <$> _tavBinaryValue,
                  ("DatetimeValue" .=) <$> _tavDatetimeValue,
                  ("NumberValue" .=) <$> _tavNumberValue,
                  ("StringValue" .=) <$> _tavStringValue,
                  ("BooleanValue" .=) <$> _tavBooleanValue])

-- | A range of attribute values. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/objectsandlinks.html#rangefilters Range Filters> .
--
--
--
-- /See:/ 'typedAttributeValueRange' smart constructor.
data TypedAttributeValueRange = TypedAttributeValueRange'
  { _tavrEndValue   :: !(Maybe TypedAttributeValue)
  , _tavrStartValue :: !(Maybe TypedAttributeValue)
  , _tavrStartMode  :: !RangeMode
  , _tavrEndMode    :: !RangeMode
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TypedAttributeValueRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tavrEndValue' - The attribute value to terminate the range at.
--
-- * 'tavrStartValue' - The value to start the range at.
--
-- * 'tavrStartMode' - The inclusive or exclusive range start.
--
-- * 'tavrEndMode' - The inclusive or exclusive range end.
typedAttributeValueRange
    :: RangeMode -- ^ 'tavrStartMode'
    -> RangeMode -- ^ 'tavrEndMode'
    -> TypedAttributeValueRange
typedAttributeValueRange pStartMode_ pEndMode_ =
  TypedAttributeValueRange'
    { _tavrEndValue = Nothing
    , _tavrStartValue = Nothing
    , _tavrStartMode = pStartMode_
    , _tavrEndMode = pEndMode_
    }


-- | The attribute value to terminate the range at.
tavrEndValue :: Lens' TypedAttributeValueRange (Maybe TypedAttributeValue)
tavrEndValue = lens _tavrEndValue (\ s a -> s{_tavrEndValue = a})

-- | The value to start the range at.
tavrStartValue :: Lens' TypedAttributeValueRange (Maybe TypedAttributeValue)
tavrStartValue = lens _tavrStartValue (\ s a -> s{_tavrStartValue = a})

-- | The inclusive or exclusive range start.
tavrStartMode :: Lens' TypedAttributeValueRange RangeMode
tavrStartMode = lens _tavrStartMode (\ s a -> s{_tavrStartMode = a})

-- | The inclusive or exclusive range end.
tavrEndMode :: Lens' TypedAttributeValueRange RangeMode
tavrEndMode = lens _tavrEndMode (\ s a -> s{_tavrEndMode = a})

instance Hashable TypedAttributeValueRange where

instance NFData TypedAttributeValueRange where

instance ToJSON TypedAttributeValueRange where
        toJSON TypedAttributeValueRange'{..}
          = object
              (catMaybes
                 [("EndValue" .=) <$> _tavrEndValue,
                  ("StartValue" .=) <$> _tavrStartValue,
                  Just ("StartMode" .= _tavrStartMode),
                  Just ("EndMode" .= _tavrEndMode)])

-- | A typed link attribute definition.
--
--
--
-- /See:/ 'typedLinkAttributeDefinition' smart constructor.
data TypedLinkAttributeDefinition = TypedLinkAttributeDefinition'
  { _tladRules            :: !(Maybe (Map Text Rule))
  , _tladDefaultValue     :: !(Maybe TypedAttributeValue)
  , _tladIsImmutable      :: !(Maybe Bool)
  , _tladName             :: !Text
  , _tladType             :: !FacetAttributeType
  , _tladRequiredBehavior :: !RequiredAttributeBehavior
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TypedLinkAttributeDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tladRules' - Validation rules that are attached to the attribute definition.
--
-- * 'tladDefaultValue' - The default value of the attribute (if configured).
--
-- * 'tladIsImmutable' - Whether the attribute is mutable or not.
--
-- * 'tladName' - The unique name of the typed link attribute.
--
-- * 'tladType' - The type of the attribute.
--
-- * 'tladRequiredBehavior' - The required behavior of the @TypedLinkAttributeDefinition@ .
typedLinkAttributeDefinition
    :: Text -- ^ 'tladName'
    -> FacetAttributeType -- ^ 'tladType'
    -> RequiredAttributeBehavior -- ^ 'tladRequiredBehavior'
    -> TypedLinkAttributeDefinition
typedLinkAttributeDefinition pName_ pType_ pRequiredBehavior_ =
  TypedLinkAttributeDefinition'
    { _tladRules = Nothing
    , _tladDefaultValue = Nothing
    , _tladIsImmutable = Nothing
    , _tladName = pName_
    , _tladType = pType_
    , _tladRequiredBehavior = pRequiredBehavior_
    }


-- | Validation rules that are attached to the attribute definition.
tladRules :: Lens' TypedLinkAttributeDefinition (HashMap Text Rule)
tladRules = lens _tladRules (\ s a -> s{_tladRules = a}) . _Default . _Map

-- | The default value of the attribute (if configured).
tladDefaultValue :: Lens' TypedLinkAttributeDefinition (Maybe TypedAttributeValue)
tladDefaultValue = lens _tladDefaultValue (\ s a -> s{_tladDefaultValue = a})

-- | Whether the attribute is mutable or not.
tladIsImmutable :: Lens' TypedLinkAttributeDefinition (Maybe Bool)
tladIsImmutable = lens _tladIsImmutable (\ s a -> s{_tladIsImmutable = a})

-- | The unique name of the typed link attribute.
tladName :: Lens' TypedLinkAttributeDefinition Text
tladName = lens _tladName (\ s a -> s{_tladName = a})

-- | The type of the attribute.
tladType :: Lens' TypedLinkAttributeDefinition FacetAttributeType
tladType = lens _tladType (\ s a -> s{_tladType = a})

-- | The required behavior of the @TypedLinkAttributeDefinition@ .
tladRequiredBehavior :: Lens' TypedLinkAttributeDefinition RequiredAttributeBehavior
tladRequiredBehavior = lens _tladRequiredBehavior (\ s a -> s{_tladRequiredBehavior = a})

instance FromJSON TypedLinkAttributeDefinition where
        parseJSON
          = withObject "TypedLinkAttributeDefinition"
              (\ x ->
                 TypedLinkAttributeDefinition' <$>
                   (x .:? "Rules" .!= mempty) <*> (x .:? "DefaultValue")
                     <*> (x .:? "IsImmutable")
                     <*> (x .: "Name")
                     <*> (x .: "Type")
                     <*> (x .: "RequiredBehavior"))

instance Hashable TypedLinkAttributeDefinition where

instance NFData TypedLinkAttributeDefinition where

instance ToJSON TypedLinkAttributeDefinition where
        toJSON TypedLinkAttributeDefinition'{..}
          = object
              (catMaybes
                 [("Rules" .=) <$> _tladRules,
                  ("DefaultValue" .=) <$> _tladDefaultValue,
                  ("IsImmutable" .=) <$> _tladIsImmutable,
                  Just ("Name" .= _tladName),
                  Just ("Type" .= _tladType),
                  Just ("RequiredBehavior" .= _tladRequiredBehavior)])

-- | Identifies the range of attributes that are used by a specified filter.
--
--
--
-- /See:/ 'typedLinkAttributeRange' smart constructor.
data TypedLinkAttributeRange = TypedLinkAttributeRange'
  { _tlarAttributeName :: !(Maybe Text)
  , _tlarRange         :: !TypedAttributeValueRange
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TypedLinkAttributeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlarAttributeName' - The unique name of the typed link attribute.
--
-- * 'tlarRange' - The range of attribute values that are being selected.
typedLinkAttributeRange
    :: TypedAttributeValueRange -- ^ 'tlarRange'
    -> TypedLinkAttributeRange
typedLinkAttributeRange pRange_ =
  TypedLinkAttributeRange' {_tlarAttributeName = Nothing, _tlarRange = pRange_}


-- | The unique name of the typed link attribute.
tlarAttributeName :: Lens' TypedLinkAttributeRange (Maybe Text)
tlarAttributeName = lens _tlarAttributeName (\ s a -> s{_tlarAttributeName = a})

-- | The range of attribute values that are being selected.
tlarRange :: Lens' TypedLinkAttributeRange TypedAttributeValueRange
tlarRange = lens _tlarRange (\ s a -> s{_tlarRange = a})

instance Hashable TypedLinkAttributeRange where

instance NFData TypedLinkAttributeRange where

instance ToJSON TypedLinkAttributeRange where
        toJSON TypedLinkAttributeRange'{..}
          = object
              (catMaybes
                 [("AttributeName" .=) <$> _tlarAttributeName,
                  Just ("Range" .= _tlarRange)])

-- | Defines the typed links structure and its attributes. To create a typed link facet, use the 'CreateTypedLinkFacet' API.
--
--
--
-- /See:/ 'typedLinkFacet' smart constructor.
data TypedLinkFacet = TypedLinkFacet'
  { _tlfName                   :: !Text
  , _tlfAttributes             :: ![TypedLinkAttributeDefinition]
  , _tlfIdentityAttributeOrder :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TypedLinkFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlfName' - The unique name of the typed link facet.
--
-- * 'tlfAttributes' - A set of key-value pairs associated with the typed link. Typed link attributes are used when you have data values that are related to the link itself, and not to one of the two objects being linked. Identity attributes also serve to distinguish the link from others of the same type between the same objects.
--
-- * 'tlfIdentityAttributeOrder' - The set of attributes that distinguish links made from this facet from each other, in the order of significance. Listing typed links can filter on the values of these attributes. See 'ListOutgoingTypedLinks' and 'ListIncomingTypedLinks' for details.
typedLinkFacet
    :: Text -- ^ 'tlfName'
    -> TypedLinkFacet
typedLinkFacet pName_ =
  TypedLinkFacet'
    { _tlfName = pName_
    , _tlfAttributes = mempty
    , _tlfIdentityAttributeOrder = mempty
    }


-- | The unique name of the typed link facet.
tlfName :: Lens' TypedLinkFacet Text
tlfName = lens _tlfName (\ s a -> s{_tlfName = a})

-- | A set of key-value pairs associated with the typed link. Typed link attributes are used when you have data values that are related to the link itself, and not to one of the two objects being linked. Identity attributes also serve to distinguish the link from others of the same type between the same objects.
tlfAttributes :: Lens' TypedLinkFacet [TypedLinkAttributeDefinition]
tlfAttributes = lens _tlfAttributes (\ s a -> s{_tlfAttributes = a}) . _Coerce

-- | The set of attributes that distinguish links made from this facet from each other, in the order of significance. Listing typed links can filter on the values of these attributes. See 'ListOutgoingTypedLinks' and 'ListIncomingTypedLinks' for details.
tlfIdentityAttributeOrder :: Lens' TypedLinkFacet [Text]
tlfIdentityAttributeOrder = lens _tlfIdentityAttributeOrder (\ s a -> s{_tlfIdentityAttributeOrder = a}) . _Coerce

instance Hashable TypedLinkFacet where

instance NFData TypedLinkFacet where

instance ToJSON TypedLinkFacet where
        toJSON TypedLinkFacet'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _tlfName),
                  Just ("Attributes" .= _tlfAttributes),
                  Just
                    ("IdentityAttributeOrder" .=
                       _tlfIdentityAttributeOrder)])

-- | A typed link facet attribute update.
--
--
--
-- /See:/ 'typedLinkFacetAttributeUpdate' smart constructor.
data TypedLinkFacetAttributeUpdate = TypedLinkFacetAttributeUpdate'
  { _tlfauAttribute :: !TypedLinkAttributeDefinition
  , _tlfauAction    :: !UpdateActionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TypedLinkFacetAttributeUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlfauAttribute' - The attribute to update.
--
-- * 'tlfauAction' - The action to perform when updating the attribute.
typedLinkFacetAttributeUpdate
    :: TypedLinkAttributeDefinition -- ^ 'tlfauAttribute'
    -> UpdateActionType -- ^ 'tlfauAction'
    -> TypedLinkFacetAttributeUpdate
typedLinkFacetAttributeUpdate pAttribute_ pAction_ =
  TypedLinkFacetAttributeUpdate'
    {_tlfauAttribute = pAttribute_, _tlfauAction = pAction_}


-- | The attribute to update.
tlfauAttribute :: Lens' TypedLinkFacetAttributeUpdate TypedLinkAttributeDefinition
tlfauAttribute = lens _tlfauAttribute (\ s a -> s{_tlfauAttribute = a})

-- | The action to perform when updating the attribute.
tlfauAction :: Lens' TypedLinkFacetAttributeUpdate UpdateActionType
tlfauAction = lens _tlfauAction (\ s a -> s{_tlfauAction = a})

instance Hashable TypedLinkFacetAttributeUpdate where

instance NFData TypedLinkFacetAttributeUpdate where

instance ToJSON TypedLinkFacetAttributeUpdate where
        toJSON TypedLinkFacetAttributeUpdate'{..}
          = object
              (catMaybes
                 [Just ("Attribute" .= _tlfauAttribute),
                  Just ("Action" .= _tlfauAction)])

-- | Identifies the schema Amazon Resource Name (ARN) and facet name for the typed link.
--
--
--
-- /See:/ 'typedLinkSchemaAndFacetName' smart constructor.
data TypedLinkSchemaAndFacetName = TypedLinkSchemaAndFacetName'
  { _tlsafnSchemaARN     :: !Text
  , _tlsafnTypedLinkName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TypedLinkSchemaAndFacetName' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlsafnSchemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- * 'tlsafnTypedLinkName' - The unique name of the typed link facet.
typedLinkSchemaAndFacetName
    :: Text -- ^ 'tlsafnSchemaARN'
    -> Text -- ^ 'tlsafnTypedLinkName'
    -> TypedLinkSchemaAndFacetName
typedLinkSchemaAndFacetName pSchemaARN_ pTypedLinkName_ =
  TypedLinkSchemaAndFacetName'
    {_tlsafnSchemaARN = pSchemaARN_, _tlsafnTypedLinkName = pTypedLinkName_}


-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
tlsafnSchemaARN :: Lens' TypedLinkSchemaAndFacetName Text
tlsafnSchemaARN = lens _tlsafnSchemaARN (\ s a -> s{_tlsafnSchemaARN = a})

-- | The unique name of the typed link facet.
tlsafnTypedLinkName :: Lens' TypedLinkSchemaAndFacetName Text
tlsafnTypedLinkName = lens _tlsafnTypedLinkName (\ s a -> s{_tlsafnTypedLinkName = a})

instance FromJSON TypedLinkSchemaAndFacetName where
        parseJSON
          = withObject "TypedLinkSchemaAndFacetName"
              (\ x ->
                 TypedLinkSchemaAndFacetName' <$>
                   (x .: "SchemaArn") <*> (x .: "TypedLinkName"))

instance Hashable TypedLinkSchemaAndFacetName where

instance NFData TypedLinkSchemaAndFacetName where

instance ToJSON TypedLinkSchemaAndFacetName where
        toJSON TypedLinkSchemaAndFacetName'{..}
          = object
              (catMaybes
                 [Just ("SchemaArn" .= _tlsafnSchemaARN),
                  Just ("TypedLinkName" .= _tlsafnTypedLinkName)])

-- | Contains all the information that is used to uniquely identify a typed link. The parameters discussed in this topic are used to uniquely specify the typed link being operated on. The 'AttachTypedLink' API returns a typed link specifier while the 'DetachTypedLink' API accepts one as input. Similarly, the 'ListIncomingTypedLinks' and 'ListOutgoingTypedLinks' API operations provide typed link specifiers as output. You can also construct a typed link specifier from scratch.
--
--
--
-- /See:/ 'typedLinkSpecifier' smart constructor.
data TypedLinkSpecifier = TypedLinkSpecifier'
  { _tlsTypedLinkFacet          :: !TypedLinkSchemaAndFacetName
  , _tlsSourceObjectReference   :: !ObjectReference
  , _tlsTargetObjectReference   :: !ObjectReference
  , _tlsIdentityAttributeValues :: ![AttributeNameAndValue]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TypedLinkSpecifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlsTypedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
--
-- * 'tlsSourceObjectReference' - Identifies the source object that the typed link will attach to.
--
-- * 'tlsTargetObjectReference' - Identifies the target object that the typed link will attach to.
--
-- * 'tlsIdentityAttributeValues' - Identifies the attribute value to update.
typedLinkSpecifier
    :: TypedLinkSchemaAndFacetName -- ^ 'tlsTypedLinkFacet'
    -> ObjectReference -- ^ 'tlsSourceObjectReference'
    -> ObjectReference -- ^ 'tlsTargetObjectReference'
    -> TypedLinkSpecifier
typedLinkSpecifier pTypedLinkFacet_ pSourceObjectReference_ pTargetObjectReference_ =
  TypedLinkSpecifier'
    { _tlsTypedLinkFacet = pTypedLinkFacet_
    , _tlsSourceObjectReference = pSourceObjectReference_
    , _tlsTargetObjectReference = pTargetObjectReference_
    , _tlsIdentityAttributeValues = mempty
    }


-- | Identifies the typed link facet that is associated with the typed link.
tlsTypedLinkFacet :: Lens' TypedLinkSpecifier TypedLinkSchemaAndFacetName
tlsTypedLinkFacet = lens _tlsTypedLinkFacet (\ s a -> s{_tlsTypedLinkFacet = a})

-- | Identifies the source object that the typed link will attach to.
tlsSourceObjectReference :: Lens' TypedLinkSpecifier ObjectReference
tlsSourceObjectReference = lens _tlsSourceObjectReference (\ s a -> s{_tlsSourceObjectReference = a})

-- | Identifies the target object that the typed link will attach to.
tlsTargetObjectReference :: Lens' TypedLinkSpecifier ObjectReference
tlsTargetObjectReference = lens _tlsTargetObjectReference (\ s a -> s{_tlsTargetObjectReference = a})

-- | Identifies the attribute value to update.
tlsIdentityAttributeValues :: Lens' TypedLinkSpecifier [AttributeNameAndValue]
tlsIdentityAttributeValues = lens _tlsIdentityAttributeValues (\ s a -> s{_tlsIdentityAttributeValues = a}) . _Coerce

instance FromJSON TypedLinkSpecifier where
        parseJSON
          = withObject "TypedLinkSpecifier"
              (\ x ->
                 TypedLinkSpecifier' <$>
                   (x .: "TypedLinkFacet") <*>
                     (x .: "SourceObjectReference")
                     <*> (x .: "TargetObjectReference")
                     <*> (x .:? "IdentityAttributeValues" .!= mempty))

instance Hashable TypedLinkSpecifier where

instance NFData TypedLinkSpecifier where

instance ToJSON TypedLinkSpecifier where
        toJSON TypedLinkSpecifier'{..}
          = object
              (catMaybes
                 [Just ("TypedLinkFacet" .= _tlsTypedLinkFacet),
                  Just
                    ("SourceObjectReference" .=
                       _tlsSourceObjectReference),
                  Just
                    ("TargetObjectReference" .=
                       _tlsTargetObjectReference),
                  Just
                    ("IdentityAttributeValues" .=
                       _tlsIdentityAttributeValues)])
