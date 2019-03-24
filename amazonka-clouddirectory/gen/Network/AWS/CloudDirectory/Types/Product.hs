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

-- | Attaches a policy object to a regular object inside a 'BatchRead' operation.
