{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.Product where

import           Network.AWS.CloudDirectory.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | A unique identifier for an attribute.
--
--
--
-- /See:/ 'attributeKey' smart constructor.
data AttributeKey = AttributeKey'
    { _akSchemaARN :: !Text
    , _akFacetName :: !Text
    , _akName      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttributeKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akSchemaARN' - The ARN of the schema that contains the facet and attribute.
--
-- * 'akFacetName' - The name of the facet the attribute exists within.
--
-- * 'akName' - The name of the attribute.
attributeKey
    :: Text -- ^ 'akSchemaARN'
    -> Text -- ^ 'akFacetName'
    -> Text -- ^ 'akName'
    -> AttributeKey
attributeKey pSchemaARN_ pFacetName_ pName_ =
    AttributeKey'
    { _akSchemaARN = pSchemaARN_
    , _akFacetName = pFacetName_
    , _akName = pName_
    }

-- | The ARN of the schema that contains the facet and attribute.
akSchemaARN :: Lens' AttributeKey Text
akSchemaARN = lens _akSchemaARN (\ s a -> s{_akSchemaARN = a});

-- | The name of the facet the attribute exists within.
akFacetName :: Lens' AttributeKey Text
akFacetName = lens _akFacetName (\ s a -> s{_akFacetName = a});

-- | The name of the attribute.
akName :: Lens' AttributeKey Text
akName = lens _akName (\ s a -> s{_akName = a});

instance FromJSON AttributeKey where
        parseJSON
          = withObject "AttributeKey"
              (\ x ->
                 AttributeKey' <$>
                   (x .: "SchemaArn") <*> (x .: "FacetName") <*>
                     (x .: "Name"))

instance Hashable AttributeKey

instance NFData AttributeKey

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    AttributeKeyAndValue'
    { _akavKey = pKey_
    , _akavValue = pValue_
    }

-- | The key of the attribute.
akavKey :: Lens' AttributeKeyAndValue AttributeKey
akavKey = lens _akavKey (\ s a -> s{_akavKey = a});

-- | The value of the attribute.
akavValue :: Lens' AttributeKeyAndValue TypedAttributeValue
akavValue = lens _akavValue (\ s a -> s{_akavValue = a});

instance FromJSON AttributeKeyAndValue where
        parseJSON
          = withObject "AttributeKeyAndValue"
              (\ x ->
                 AttributeKeyAndValue' <$>
                   (x .: "Key") <*> (x .: "Value"))

instance Hashable AttributeKeyAndValue

instance NFData AttributeKeyAndValue

instance ToJSON AttributeKeyAndValue where
        toJSON AttributeKeyAndValue'{..}
          = object
              (catMaybes
                 [Just ("Key" .= _akavKey),
                  Just ("Value" .= _akavValue)])

-- | Represents the output of a batch add facet to object operation.
--
--
--
-- /See:/ 'batchAddFacetToObject' smart constructor.
data BatchAddFacetToObject = BatchAddFacetToObject'
    { _baftoSchemaFacet         :: !SchemaFacet
    , _baftoObjectAttributeList :: ![AttributeKeyAndValue]
    , _baftoObjectReference     :: !ObjectReference
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
baftoSchemaFacet = lens _baftoSchemaFacet (\ s a -> s{_baftoSchemaFacet = a});

-- | The attributes to set on the object.
baftoObjectAttributeList :: Lens' BatchAddFacetToObject [AttributeKeyAndValue]
baftoObjectAttributeList = lens _baftoObjectAttributeList (\ s a -> s{_baftoObjectAttributeList = a}) . _Coerce;

-- | A reference to the object being mutated.
baftoObjectReference :: Lens' BatchAddFacetToObject ObjectReference
baftoObjectReference = lens _baftoObjectReference (\ s a -> s{_baftoObjectReference = a});

instance Hashable BatchAddFacetToObject

instance NFData BatchAddFacetToObject

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchAddFacetToObjectResponse' with the minimum fields required to make a request.
--
batchAddFacetToObjectResponse
    :: BatchAddFacetToObjectResponse
batchAddFacetToObjectResponse = BatchAddFacetToObjectResponse'

instance FromJSON BatchAddFacetToObjectResponse where
        parseJSON
          = withObject "BatchAddFacetToObjectResponse"
              (\ x -> pure BatchAddFacetToObjectResponse')

instance Hashable BatchAddFacetToObjectResponse

instance NFData BatchAddFacetToObjectResponse

-- | Represents the output of an AttachObject operation.
--
--
--
-- /See:/ 'batchAttachObject' smart constructor.
data BatchAttachObject = BatchAttachObject'
    { _baoParentReference :: !ObjectReference
    , _baoChildReference  :: !ObjectReference
    , _baoLinkName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchAttachObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baoParentReference' - Parent object reference.
--
-- * 'baoChildReference' - Child object reference to be attached to the object.
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

-- | Parent object reference.
baoParentReference :: Lens' BatchAttachObject ObjectReference
baoParentReference = lens _baoParentReference (\ s a -> s{_baoParentReference = a});

-- | Child object reference to be attached to the object.
baoChildReference :: Lens' BatchAttachObject ObjectReference
baoChildReference = lens _baoChildReference (\ s a -> s{_baoChildReference = a});

-- | The name of the link.
baoLinkName :: Lens' BatchAttachObject Text
baoLinkName = lens _baoLinkName (\ s a -> s{_baoLinkName = a});

instance Hashable BatchAttachObject

instance NFData BatchAttachObject

instance ToJSON BatchAttachObject where
        toJSON BatchAttachObject'{..}
          = object
              (catMaybes
                 [Just ("ParentReference" .= _baoParentReference),
                  Just ("ChildReference" .= _baoChildReference),
                  Just ("LinkName" .= _baoLinkName)])

-- | Represents the output batch AttachObject response operation.
--
--
--
-- /See:/ 'batchAttachObjectResponse' smart constructor.
newtype BatchAttachObjectResponse = BatchAttachObjectResponse'
    { _baoAttachedObjectIdentifier :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchAttachObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'baoAttachedObjectIdentifier' - The @ObjectIdentifier@ of the object that has been attached.
batchAttachObjectResponse
    :: BatchAttachObjectResponse
batchAttachObjectResponse =
    BatchAttachObjectResponse'
    { _baoAttachedObjectIdentifier = Nothing
    }

-- | The @ObjectIdentifier@ of the object that has been attached.
baoAttachedObjectIdentifier :: Lens' BatchAttachObjectResponse (Maybe Text)
baoAttachedObjectIdentifier = lens _baoAttachedObjectIdentifier (\ s a -> s{_baoAttachedObjectIdentifier = a});

instance FromJSON BatchAttachObjectResponse where
        parseJSON
          = withObject "BatchAttachObjectResponse"
              (\ x ->
                 BatchAttachObjectResponse' <$>
                   (x .:? "attachedObjectIdentifier"))

instance Hashable BatchAttachObjectResponse

instance NFData BatchAttachObjectResponse

-- | Represents the output of a CreateObject operation.
--
--
--
-- /See:/ 'batchCreateObject' smart constructor.
data BatchCreateObject = BatchCreateObject'
    { _bcoSchemaFacet         :: ![SchemaFacet]
    , _bcoObjectAttributeList :: ![AttributeKeyAndValue]
    , _bcoParentReference     :: !ObjectReference
    , _bcoLinkName            :: !Text
    , _bcoBatchReferenceName  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchCreateObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcoSchemaFacet' - List of FacetArns that will be associated with the object. For more information, see 'arns' .
--
-- * 'bcoObjectAttributeList' - Attribute map, which contains an attribute ARN as the key and attribute value as the map value.
--
-- * 'bcoParentReference' - If specified, the parent reference to which this object will be attached.
--
-- * 'bcoLinkName' - The name of the link.
--
-- * 'bcoBatchReferenceName' - The batch reference name. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#batches Batches> for more information.
batchCreateObject
    :: ObjectReference -- ^ 'bcoParentReference'
    -> Text -- ^ 'bcoLinkName'
    -> Text -- ^ 'bcoBatchReferenceName'
    -> BatchCreateObject
batchCreateObject pParentReference_ pLinkName_ pBatchReferenceName_ =
    BatchCreateObject'
    { _bcoSchemaFacet = mempty
    , _bcoObjectAttributeList = mempty
    , _bcoParentReference = pParentReference_
    , _bcoLinkName = pLinkName_
    , _bcoBatchReferenceName = pBatchReferenceName_
    }

-- | List of FacetArns that will be associated with the object. For more information, see 'arns' .
bcoSchemaFacet :: Lens' BatchCreateObject [SchemaFacet]
bcoSchemaFacet = lens _bcoSchemaFacet (\ s a -> s{_bcoSchemaFacet = a}) . _Coerce;

-- | Attribute map, which contains an attribute ARN as the key and attribute value as the map value.
bcoObjectAttributeList :: Lens' BatchCreateObject [AttributeKeyAndValue]
bcoObjectAttributeList = lens _bcoObjectAttributeList (\ s a -> s{_bcoObjectAttributeList = a}) . _Coerce;

-- | If specified, the parent reference to which this object will be attached.
bcoParentReference :: Lens' BatchCreateObject ObjectReference
bcoParentReference = lens _bcoParentReference (\ s a -> s{_bcoParentReference = a});

-- | The name of the link.
bcoLinkName :: Lens' BatchCreateObject Text
bcoLinkName = lens _bcoLinkName (\ s a -> s{_bcoLinkName = a});

-- | The batch reference name. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#batches Batches> for more information.
bcoBatchReferenceName :: Lens' BatchCreateObject Text
bcoBatchReferenceName = lens _bcoBatchReferenceName (\ s a -> s{_bcoBatchReferenceName = a});

instance Hashable BatchCreateObject

instance NFData BatchCreateObject

instance ToJSON BatchCreateObject where
        toJSON BatchCreateObject'{..}
          = object
              (catMaybes
                 [Just ("SchemaFacet" .= _bcoSchemaFacet),
                  Just
                    ("ObjectAttributeList" .= _bcoObjectAttributeList),
                  Just ("ParentReference" .= _bcoParentReference),
                  Just ("LinkName" .= _bcoLinkName),
                  Just
                    ("BatchReferenceName" .= _bcoBatchReferenceName)])

-- | Represents the output of a @CreateObject@ response operation.
--
--
--
-- /See:/ 'batchCreateObjectResponse' smart constructor.
newtype BatchCreateObjectResponse = BatchCreateObjectResponse'
    { _bcoObjectIdentifier :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchCreateObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bcoObjectIdentifier' - ID associated with the object.
batchCreateObjectResponse
    :: BatchCreateObjectResponse
batchCreateObjectResponse =
    BatchCreateObjectResponse'
    { _bcoObjectIdentifier = Nothing
    }

-- | ID associated with the object.
bcoObjectIdentifier :: Lens' BatchCreateObjectResponse (Maybe Text)
bcoObjectIdentifier = lens _bcoObjectIdentifier (\ s a -> s{_bcoObjectIdentifier = a});

instance FromJSON BatchCreateObjectResponse where
        parseJSON
          = withObject "BatchCreateObjectResponse"
              (\ x ->
                 BatchCreateObjectResponse' <$>
                   (x .:? "ObjectIdentifier"))

instance Hashable BatchCreateObjectResponse

instance NFData BatchCreateObjectResponse

-- | Represents the output of a @DeleteObject@ operation.
--
--
--
-- /See:/ 'batchDeleteObject' smart constructor.
newtype BatchDeleteObject = BatchDeleteObject'
    { _bdoObjectReference :: ObjectReference
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchDeleteObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdoObjectReference' - Reference that identifies the object.
batchDeleteObject
    :: ObjectReference -- ^ 'bdoObjectReference'
    -> BatchDeleteObject
batchDeleteObject pObjectReference_ =
    BatchDeleteObject'
    { _bdoObjectReference = pObjectReference_
    }

-- | Reference that identifies the object.
bdoObjectReference :: Lens' BatchDeleteObject ObjectReference
bdoObjectReference = lens _bdoObjectReference (\ s a -> s{_bdoObjectReference = a});

instance Hashable BatchDeleteObject

instance NFData BatchDeleteObject

instance ToJSON BatchDeleteObject where
        toJSON BatchDeleteObject'{..}
          = object
              (catMaybes
                 [Just ("ObjectReference" .= _bdoObjectReference)])

-- | Represents the output of a @DeleteObject@ response operation.
--
--
--
-- /See:/ 'batchDeleteObjectResponse' smart constructor.
data BatchDeleteObjectResponse =
    BatchDeleteObjectResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchDeleteObjectResponse' with the minimum fields required to make a request.
--
batchDeleteObjectResponse
    :: BatchDeleteObjectResponse
batchDeleteObjectResponse = BatchDeleteObjectResponse'

instance FromJSON BatchDeleteObjectResponse where
        parseJSON
          = withObject "BatchDeleteObjectResponse"
              (\ x -> pure BatchDeleteObjectResponse')

instance Hashable BatchDeleteObjectResponse

instance NFData BatchDeleteObjectResponse

-- | Represents the output of a @DetachObject@ operation.
--
--
--
-- /See:/ 'batchDetachObject' smart constructor.
data BatchDetachObject = BatchDetachObject'
    { _bdoParentReference    :: !ObjectReference
    , _bdoLinkName           :: !Text
    , _bdoBatchReferenceName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchDetachObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdoParentReference' - Parent reference from which the object with the specified link name is detached.
--
-- * 'bdoLinkName' - The name of the link.
--
-- * 'bdoBatchReferenceName' - The batch reference name. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#batches Batches> for more information.
batchDetachObject
    :: ObjectReference -- ^ 'bdoParentReference'
    -> Text -- ^ 'bdoLinkName'
    -> Text -- ^ 'bdoBatchReferenceName'
    -> BatchDetachObject
batchDetachObject pParentReference_ pLinkName_ pBatchReferenceName_ =
    BatchDetachObject'
    { _bdoParentReference = pParentReference_
    , _bdoLinkName = pLinkName_
    , _bdoBatchReferenceName = pBatchReferenceName_
    }

-- | Parent reference from which the object with the specified link name is detached.
bdoParentReference :: Lens' BatchDetachObject ObjectReference
bdoParentReference = lens _bdoParentReference (\ s a -> s{_bdoParentReference = a});

-- | The name of the link.
bdoLinkName :: Lens' BatchDetachObject Text
bdoLinkName = lens _bdoLinkName (\ s a -> s{_bdoLinkName = a});

-- | The batch reference name. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#batches Batches> for more information.
bdoBatchReferenceName :: Lens' BatchDetachObject Text
bdoBatchReferenceName = lens _bdoBatchReferenceName (\ s a -> s{_bdoBatchReferenceName = a});

instance Hashable BatchDetachObject

instance NFData BatchDetachObject

instance ToJSON BatchDetachObject where
        toJSON BatchDetachObject'{..}
          = object
              (catMaybes
                 [Just ("ParentReference" .= _bdoParentReference),
                  Just ("LinkName" .= _bdoLinkName),
                  Just
                    ("BatchReferenceName" .= _bdoBatchReferenceName)])

-- | Represents the output of a @DetachObject@ response operation.
--
--
--
-- /See:/ 'batchDetachObjectResponse' smart constructor.
newtype BatchDetachObjectResponse = BatchDetachObjectResponse'
    { _bdoDetachedObjectIdentifier :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchDetachObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdoDetachedObjectIdentifier' - The @ObjectIdentifier@ of the detached object.
batchDetachObjectResponse
    :: BatchDetachObjectResponse
batchDetachObjectResponse =
    BatchDetachObjectResponse'
    { _bdoDetachedObjectIdentifier = Nothing
    }

-- | The @ObjectIdentifier@ of the detached object.
bdoDetachedObjectIdentifier :: Lens' BatchDetachObjectResponse (Maybe Text)
bdoDetachedObjectIdentifier = lens _bdoDetachedObjectIdentifier (\ s a -> s{_bdoDetachedObjectIdentifier = a});

instance FromJSON BatchDetachObjectResponse where
        parseJSON
          = withObject "BatchDetachObjectResponse"
              (\ x ->
                 BatchDetachObjectResponse' <$>
                   (x .:? "detachedObjectIdentifier"))

instance Hashable BatchDetachObjectResponse

instance NFData BatchDetachObjectResponse

-- | Represents the output of a @ListObjectAttributes@ operation.
--
--
--
-- /See:/ 'batchListObjectAttributes' smart constructor.
data BatchListObjectAttributes = BatchListObjectAttributes'
    { _bloaNextToken       :: !(Maybe Text)
    , _bloaMaxResults      :: !(Maybe Nat)
    , _bloaObjectReference :: !ObjectReference
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchListObjectAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bloaNextToken' - The pagination token.
--
-- * 'bloaMaxResults' - Maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'bloaObjectReference' - Reference of the object whose attributes need to be listed.
batchListObjectAttributes
    :: ObjectReference -- ^ 'bloaObjectReference'
    -> BatchListObjectAttributes
batchListObjectAttributes pObjectReference_ =
    BatchListObjectAttributes'
    { _bloaNextToken = Nothing
    , _bloaMaxResults = Nothing
    , _bloaObjectReference = pObjectReference_
    }

-- | The pagination token.
bloaNextToken :: Lens' BatchListObjectAttributes (Maybe Text)
bloaNextToken = lens _bloaNextToken (\ s a -> s{_bloaNextToken = a});

-- | Maximum number of items to be retrieved in a single call. This is an approximate number.
bloaMaxResults :: Lens' BatchListObjectAttributes (Maybe Natural)
bloaMaxResults = lens _bloaMaxResults (\ s a -> s{_bloaMaxResults = a}) . mapping _Nat;

-- | Reference of the object whose attributes need to be listed.
bloaObjectReference :: Lens' BatchListObjectAttributes ObjectReference
bloaObjectReference = lens _bloaObjectReference (\ s a -> s{_bloaObjectReference = a});

instance Hashable BatchListObjectAttributes

instance NFData BatchListObjectAttributes

instance ToJSON BatchListObjectAttributes where
        toJSON BatchListObjectAttributes'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _bloaNextToken,
                  ("MaxResults" .=) <$> _bloaMaxResults,
                  Just ("ObjectReference" .= _bloaObjectReference)])

-- | Represents the output of a @ListObjectAttributes@ response operation.
--
--
--
-- /See:/ 'batchListObjectAttributesResponse' smart constructor.
data BatchListObjectAttributesResponse = BatchListObjectAttributesResponse'
    { _bNextToken  :: !(Maybe Text)
    , _bAttributes :: !(Maybe [AttributeKeyAndValue])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchListObjectAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bNextToken' - The pagination token.
--
-- * 'bAttributes' - Attributes map associated with the object. @AttributeArn@ is the key; attribute value is the value.
batchListObjectAttributesResponse
    :: BatchListObjectAttributesResponse
batchListObjectAttributesResponse =
    BatchListObjectAttributesResponse'
    { _bNextToken = Nothing
    , _bAttributes = Nothing
    }

-- | The pagination token.
bNextToken :: Lens' BatchListObjectAttributesResponse (Maybe Text)
bNextToken = lens _bNextToken (\ s a -> s{_bNextToken = a});

-- | Attributes map associated with the object. @AttributeArn@ is the key; attribute value is the value.
bAttributes :: Lens' BatchListObjectAttributesResponse [AttributeKeyAndValue]
bAttributes = lens _bAttributes (\ s a -> s{_bAttributes = a}) . _Default . _Coerce;

instance FromJSON BatchListObjectAttributesResponse
         where
        parseJSON
          = withObject "BatchListObjectAttributesResponse"
              (\ x ->
                 BatchListObjectAttributesResponse' <$>
                   (x .:? "NextToken") <*>
                     (x .:? "Attributes" .!= mempty))

instance Hashable BatchListObjectAttributesResponse

instance NFData BatchListObjectAttributesResponse

-- | Represents the output of a @ListObjectChildren@ operation.
--
--
--
-- /See:/ 'batchListObjectChildren' smart constructor.
data BatchListObjectChildren = BatchListObjectChildren'
    { _batNextToken       :: !(Maybe Text)
    , _batMaxResults      :: !(Maybe Nat)
    , _batObjectReference :: !ObjectReference
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchListObjectChildren' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batNextToken' - The pagination token.
--
-- * 'batMaxResults' - Maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'batObjectReference' - Reference of the object for which child objects are being listed.
batchListObjectChildren
    :: ObjectReference -- ^ 'batObjectReference'
    -> BatchListObjectChildren
batchListObjectChildren pObjectReference_ =
    BatchListObjectChildren'
    { _batNextToken = Nothing
    , _batMaxResults = Nothing
    , _batObjectReference = pObjectReference_
    }

-- | The pagination token.
batNextToken :: Lens' BatchListObjectChildren (Maybe Text)
batNextToken = lens _batNextToken (\ s a -> s{_batNextToken = a});

-- | Maximum number of items to be retrieved in a single call. This is an approximate number.
batMaxResults :: Lens' BatchListObjectChildren (Maybe Natural)
batMaxResults = lens _batMaxResults (\ s a -> s{_batMaxResults = a}) . mapping _Nat;

-- | Reference of the object for which child objects are being listed.
batObjectReference :: Lens' BatchListObjectChildren ObjectReference
batObjectReference = lens _batObjectReference (\ s a -> s{_batObjectReference = a});

instance Hashable BatchListObjectChildren

instance NFData BatchListObjectChildren

instance ToJSON BatchListObjectChildren where
        toJSON BatchListObjectChildren'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _batNextToken,
                  ("MaxResults" .=) <$> _batMaxResults,
                  Just ("ObjectReference" .= _batObjectReference)])

-- | Represents the output of a @ListObjectChildren@ response operation.
--
--
--
-- /See:/ 'batchListObjectChildrenResponse' smart constructor.
data BatchListObjectChildrenResponse = BatchListObjectChildrenResponse'
    { _blocChildren  :: !(Maybe (Map Text Text))
    , _blocNextToken :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchListObjectChildrenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blocChildren' - Children structure, which is a map with key as the @LinkName@ and @ObjectIdentifier@ as the value.
--
-- * 'blocNextToken' - The pagination token.
batchListObjectChildrenResponse
    :: BatchListObjectChildrenResponse
batchListObjectChildrenResponse =
    BatchListObjectChildrenResponse'
    { _blocChildren = Nothing
    , _blocNextToken = Nothing
    }

-- | Children structure, which is a map with key as the @LinkName@ and @ObjectIdentifier@ as the value.
blocChildren :: Lens' BatchListObjectChildrenResponse (HashMap Text Text)
blocChildren = lens _blocChildren (\ s a -> s{_blocChildren = a}) . _Default . _Map;

-- | The pagination token.
blocNextToken :: Lens' BatchListObjectChildrenResponse (Maybe Text)
blocNextToken = lens _blocNextToken (\ s a -> s{_blocNextToken = a});

instance FromJSON BatchListObjectChildrenResponse
         where
        parseJSON
          = withObject "BatchListObjectChildrenResponse"
              (\ x ->
                 BatchListObjectChildrenResponse' <$>
                   (x .:? "Children" .!= mempty) <*>
                     (x .:? "NextToken"))

instance Hashable BatchListObjectChildrenResponse

instance NFData BatchListObjectChildrenResponse

-- | Batch Read Exception structure, which contains exception type and message.
--
--
--
-- /See:/ 'batchReadException' smart constructor.
data BatchReadException = BatchReadException'
    { _breType    :: !(Maybe BatchReadExceptionType)
    , _breMessage :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchReadException' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'breType' - Type of exception, such as @InvalidArnException@ .
--
-- * 'breMessage' - Exception message associated with the failure.
batchReadException
    :: BatchReadException
batchReadException =
    BatchReadException'
    { _breType = Nothing
    , _breMessage = Nothing
    }

-- | Type of exception, such as @InvalidArnException@ .
breType :: Lens' BatchReadException (Maybe BatchReadExceptionType)
breType = lens _breType (\ s a -> s{_breType = a});

-- | Exception message associated with the failure.
breMessage :: Lens' BatchReadException (Maybe Text)
breMessage = lens _breMessage (\ s a -> s{_breMessage = a});

instance FromJSON BatchReadException where
        parseJSON
          = withObject "BatchReadException"
              (\ x ->
                 BatchReadException' <$>
                   (x .:? "Type") <*> (x .:? "Message"))

instance Hashable BatchReadException

instance NFData BatchReadException

-- | Represents the output of a @BatchRead@ operation.
--
--
--
-- /See:/ 'batchReadOperation' smart constructor.
data BatchReadOperation = BatchReadOperation'
    { _broListObjectAttributes :: !(Maybe BatchListObjectAttributes)
    , _broListObjectChildren   :: !(Maybe BatchListObjectChildren)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchReadOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'broListObjectAttributes' - Lists all attributes associated with an object.
--
-- * 'broListObjectChildren' - Returns a paginated list of child objects that are associated with a given object.
batchReadOperation
    :: BatchReadOperation
batchReadOperation =
    BatchReadOperation'
    { _broListObjectAttributes = Nothing
    , _broListObjectChildren = Nothing
    }

-- | Lists all attributes associated with an object.
broListObjectAttributes :: Lens' BatchReadOperation (Maybe BatchListObjectAttributes)
broListObjectAttributes = lens _broListObjectAttributes (\ s a -> s{_broListObjectAttributes = a});

-- | Returns a paginated list of child objects that are associated with a given object.
broListObjectChildren :: Lens' BatchReadOperation (Maybe BatchListObjectChildren)
broListObjectChildren = lens _broListObjectChildren (\ s a -> s{_broListObjectChildren = a});

instance Hashable BatchReadOperation

instance NFData BatchReadOperation

instance ToJSON BatchReadOperation where
        toJSON BatchReadOperation'{..}
          = object
              (catMaybes
                 [("ListObjectAttributes" .=) <$>
                    _broListObjectAttributes,
                  ("ListObjectChildren" .=) <$>
                    _broListObjectChildren])

-- | Represents the output of a @BatchRead@ response operation.
--
--
--
-- /See:/ 'batchReadOperationResponse' smart constructor.
data BatchReadOperationResponse = BatchReadOperationResponse'
    { _broExceptionResponse  :: !(Maybe BatchReadException)
    , _broSuccessfulResponse :: !(Maybe BatchReadSuccessfulResponse)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _broExceptionResponse = Nothing
    , _broSuccessfulResponse = Nothing
    }

-- | Identifies which operation in a batch has failed.
broExceptionResponse :: Lens' BatchReadOperationResponse (Maybe BatchReadException)
broExceptionResponse = lens _broExceptionResponse (\ s a -> s{_broExceptionResponse = a});

-- | Identifies which operation in a batch has succeeded.
broSuccessfulResponse :: Lens' BatchReadOperationResponse (Maybe BatchReadSuccessfulResponse)
broSuccessfulResponse = lens _broSuccessfulResponse (\ s a -> s{_broSuccessfulResponse = a});

instance FromJSON BatchReadOperationResponse where
        parseJSON
          = withObject "BatchReadOperationResponse"
              (\ x ->
                 BatchReadOperationResponse' <$>
                   (x .:? "ExceptionResponse") <*>
                     (x .:? "SuccessfulResponse"))

instance Hashable BatchReadOperationResponse

instance NFData BatchReadOperationResponse

-- | Represents the output of a @BatchRead@ success response operation.
--
--
--
-- /See:/ 'batchReadSuccessfulResponse' smart constructor.
data BatchReadSuccessfulResponse = BatchReadSuccessfulResponse'
    { _brsListObjectAttributes :: !(Maybe BatchListObjectAttributesResponse)
    , _brsListObjectChildren   :: !(Maybe BatchListObjectChildrenResponse)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchReadSuccessfulResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brsListObjectAttributes' - Lists all attributes associated with an object.
--
-- * 'brsListObjectChildren' - Returns a paginated list of child objects associated with a given object.
batchReadSuccessfulResponse
    :: BatchReadSuccessfulResponse
batchReadSuccessfulResponse =
    BatchReadSuccessfulResponse'
    { _brsListObjectAttributes = Nothing
    , _brsListObjectChildren = Nothing
    }

-- | Lists all attributes associated with an object.
brsListObjectAttributes :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectAttributesResponse)
brsListObjectAttributes = lens _brsListObjectAttributes (\ s a -> s{_brsListObjectAttributes = a});

-- | Returns a paginated list of child objects associated with a given object.
brsListObjectChildren :: Lens' BatchReadSuccessfulResponse (Maybe BatchListObjectChildrenResponse)
brsListObjectChildren = lens _brsListObjectChildren (\ s a -> s{_brsListObjectChildren = a});

instance FromJSON BatchReadSuccessfulResponse where
        parseJSON
          = withObject "BatchReadSuccessfulResponse"
              (\ x ->
                 BatchReadSuccessfulResponse' <$>
                   (x .:? "ListObjectAttributes") <*>
                     (x .:? "ListObjectChildren"))

instance Hashable BatchReadSuccessfulResponse

instance NFData BatchReadSuccessfulResponse

-- | Batch operation to remove a facet from an object.
--
--
--
-- /See:/ 'batchRemoveFacetFromObject' smart constructor.
data BatchRemoveFacetFromObject = BatchRemoveFacetFromObject'
    { _brffoSchemaFacet     :: !SchemaFacet
    , _brffoObjectReference :: !ObjectReference
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
brffoSchemaFacet = lens _brffoSchemaFacet (\ s a -> s{_brffoSchemaFacet = a});

-- | A reference to the object whose facet will be removed.
brffoObjectReference :: Lens' BatchRemoveFacetFromObject ObjectReference
brffoObjectReference = lens _brffoObjectReference (\ s a -> s{_brffoObjectReference = a});

instance Hashable BatchRemoveFacetFromObject

instance NFData BatchRemoveFacetFromObject

instance ToJSON BatchRemoveFacetFromObject where
        toJSON BatchRemoveFacetFromObject'{..}
          = object
              (catMaybes
                 [Just ("SchemaFacet" .= _brffoSchemaFacet),
                  Just ("ObjectReference" .= _brffoObjectReference)])

-- | Empty result representing success.
--
--
--
-- /See:/ 'batchRemoveFacetFromObjectResponse' smart constructor.
data BatchRemoveFacetFromObjectResponse =
    BatchRemoveFacetFromObjectResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

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

instance NFData BatchRemoveFacetFromObjectResponse

-- | Represents the output of a @BatchUpdate@ operation.
--
--
--
-- /See:/ 'batchUpdateObjectAttributes' smart constructor.
data BatchUpdateObjectAttributes = BatchUpdateObjectAttributes'
    { _buoaObjectReference  :: !ObjectReference
    , _buoaAttributeUpdates :: ![ObjectAttributeUpdate]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _buoaObjectReference = pObjectReference_
    , _buoaAttributeUpdates = mempty
    }

-- | Reference that identifies the object.
buoaObjectReference :: Lens' BatchUpdateObjectAttributes ObjectReference
buoaObjectReference = lens _buoaObjectReference (\ s a -> s{_buoaObjectReference = a});

-- | Attributes update structure.
buoaAttributeUpdates :: Lens' BatchUpdateObjectAttributes [ObjectAttributeUpdate]
buoaAttributeUpdates = lens _buoaAttributeUpdates (\ s a -> s{_buoaAttributeUpdates = a}) . _Coerce;

instance Hashable BatchUpdateObjectAttributes

instance NFData BatchUpdateObjectAttributes

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchUpdateObjectAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'buoaObjectIdentifier' - ID associated with the object.
batchUpdateObjectAttributesResponse
    :: BatchUpdateObjectAttributesResponse
batchUpdateObjectAttributesResponse =
    BatchUpdateObjectAttributesResponse'
    { _buoaObjectIdentifier = Nothing
    }

-- | ID associated with the object.
buoaObjectIdentifier :: Lens' BatchUpdateObjectAttributesResponse (Maybe Text)
buoaObjectIdentifier = lens _buoaObjectIdentifier (\ s a -> s{_buoaObjectIdentifier = a});

instance FromJSON BatchUpdateObjectAttributesResponse
         where
        parseJSON
          = withObject "BatchUpdateObjectAttributesResponse"
              (\ x ->
                 BatchUpdateObjectAttributesResponse' <$>
                   (x .:? "ObjectIdentifier"))

instance Hashable BatchUpdateObjectAttributesResponse

instance NFData BatchUpdateObjectAttributesResponse

-- | Represents the output of a @BatchWrite@ operation.
--
--
--
-- /See:/ 'batchWriteOperation' smart constructor.
data BatchWriteOperation = BatchWriteOperation'
    { _bDeleteObject           :: !(Maybe BatchDeleteObject)
    , _bRemoveFacetFromObject  :: !(Maybe BatchRemoveFacetFromObject)
    , _bAttachObject           :: !(Maybe BatchAttachObject)
    , _bCreateObject           :: !(Maybe BatchCreateObject)
    , _bDetachObject           :: !(Maybe BatchDetachObject)
    , _bAddFacetToObject       :: !(Maybe BatchAddFacetToObject)
    , _bUpdateObjectAttributes :: !(Maybe BatchUpdateObjectAttributes)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchWriteOperation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bDeleteObject' - Deletes an object in a 'Directory' .
--
-- * 'bRemoveFacetFromObject' - Batch operation removing a facet from an object.
--
-- * 'bAttachObject' - Attaches an object to a 'Directory' .
--
-- * 'bCreateObject' - Creates an object.
--
-- * 'bDetachObject' - Detaches an object from a 'Directory' .
--
-- * 'bAddFacetToObject' - Batch operation adding a facet to an object.
--
-- * 'bUpdateObjectAttributes' - Update a given object's attributes.
batchWriteOperation
    :: BatchWriteOperation
batchWriteOperation =
    BatchWriteOperation'
    { _bDeleteObject = Nothing
    , _bRemoveFacetFromObject = Nothing
    , _bAttachObject = Nothing
    , _bCreateObject = Nothing
    , _bDetachObject = Nothing
    , _bAddFacetToObject = Nothing
    , _bUpdateObjectAttributes = Nothing
    }

-- | Deletes an object in a 'Directory' .
bDeleteObject :: Lens' BatchWriteOperation (Maybe BatchDeleteObject)
bDeleteObject = lens _bDeleteObject (\ s a -> s{_bDeleteObject = a});

-- | Batch operation removing a facet from an object.
bRemoveFacetFromObject :: Lens' BatchWriteOperation (Maybe BatchRemoveFacetFromObject)
bRemoveFacetFromObject = lens _bRemoveFacetFromObject (\ s a -> s{_bRemoveFacetFromObject = a});

-- | Attaches an object to a 'Directory' .
bAttachObject :: Lens' BatchWriteOperation (Maybe BatchAttachObject)
bAttachObject = lens _bAttachObject (\ s a -> s{_bAttachObject = a});

-- | Creates an object.
bCreateObject :: Lens' BatchWriteOperation (Maybe BatchCreateObject)
bCreateObject = lens _bCreateObject (\ s a -> s{_bCreateObject = a});

-- | Detaches an object from a 'Directory' .
bDetachObject :: Lens' BatchWriteOperation (Maybe BatchDetachObject)
bDetachObject = lens _bDetachObject (\ s a -> s{_bDetachObject = a});

-- | Batch operation adding a facet to an object.
bAddFacetToObject :: Lens' BatchWriteOperation (Maybe BatchAddFacetToObject)
bAddFacetToObject = lens _bAddFacetToObject (\ s a -> s{_bAddFacetToObject = a});

-- | Update a given object's attributes.
bUpdateObjectAttributes :: Lens' BatchWriteOperation (Maybe BatchUpdateObjectAttributes)
bUpdateObjectAttributes = lens _bUpdateObjectAttributes (\ s a -> s{_bUpdateObjectAttributes = a});

instance Hashable BatchWriteOperation

instance NFData BatchWriteOperation

instance ToJSON BatchWriteOperation where
        toJSON BatchWriteOperation'{..}
          = object
              (catMaybes
                 [("DeleteObject" .=) <$> _bDeleteObject,
                  ("RemoveFacetFromObject" .=) <$>
                    _bRemoveFacetFromObject,
                  ("AttachObject" .=) <$> _bAttachObject,
                  ("CreateObject" .=) <$> _bCreateObject,
                  ("DetachObject" .=) <$> _bDetachObject,
                  ("AddFacetToObject" .=) <$> _bAddFacetToObject,
                  ("UpdateObjectAttributes" .=) <$>
                    _bUpdateObjectAttributes])

-- | Represents the output of a @BatchWrite@ response operation.
--
--
--
-- /See:/ 'batchWriteOperationResponse' smart constructor.
data BatchWriteOperationResponse = BatchWriteOperationResponse'
    { _bwoDeleteObject           :: !(Maybe BatchDeleteObjectResponse)
    , _bwoRemoveFacetFromObject  :: !(Maybe BatchRemoveFacetFromObjectResponse)
    , _bwoAttachObject           :: !(Maybe BatchAttachObjectResponse)
    , _bwoCreateObject           :: !(Maybe BatchCreateObjectResponse)
    , _bwoDetachObject           :: !(Maybe BatchDetachObjectResponse)
    , _bwoAddFacetToObject       :: !(Maybe BatchAddFacetToObjectResponse)
    , _bwoUpdateObjectAttributes :: !(Maybe BatchUpdateObjectAttributesResponse)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'BatchWriteOperationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bwoDeleteObject' - Deletes an object in a 'Directory' .
--
-- * 'bwoRemoveFacetFromObject' - Result of a batch remove facet from object operation.
--
-- * 'bwoAttachObject' - Attaches an object to a 'Directory' .
--
-- * 'bwoCreateObject' - Creates an object in a 'Directory' .
--
-- * 'bwoDetachObject' - Detaches an object from a 'Directory' .
--
-- * 'bwoAddFacetToObject' - Result of an add facet to object batch operation.
--
-- * 'bwoUpdateObjectAttributes' - Updates a given object’s attributes.
batchWriteOperationResponse
    :: BatchWriteOperationResponse
batchWriteOperationResponse =
    BatchWriteOperationResponse'
    { _bwoDeleteObject = Nothing
    , _bwoRemoveFacetFromObject = Nothing
    , _bwoAttachObject = Nothing
    , _bwoCreateObject = Nothing
    , _bwoDetachObject = Nothing
    , _bwoAddFacetToObject = Nothing
    , _bwoUpdateObjectAttributes = Nothing
    }

-- | Deletes an object in a 'Directory' .
bwoDeleteObject :: Lens' BatchWriteOperationResponse (Maybe BatchDeleteObjectResponse)
bwoDeleteObject = lens _bwoDeleteObject (\ s a -> s{_bwoDeleteObject = a});

-- | Result of a batch remove facet from object operation.
bwoRemoveFacetFromObject :: Lens' BatchWriteOperationResponse (Maybe BatchRemoveFacetFromObjectResponse)
bwoRemoveFacetFromObject = lens _bwoRemoveFacetFromObject (\ s a -> s{_bwoRemoveFacetFromObject = a});

-- | Attaches an object to a 'Directory' .
bwoAttachObject :: Lens' BatchWriteOperationResponse (Maybe BatchAttachObjectResponse)
bwoAttachObject = lens _bwoAttachObject (\ s a -> s{_bwoAttachObject = a});

-- | Creates an object in a 'Directory' .
bwoCreateObject :: Lens' BatchWriteOperationResponse (Maybe BatchCreateObjectResponse)
bwoCreateObject = lens _bwoCreateObject (\ s a -> s{_bwoCreateObject = a});

-- | Detaches an object from a 'Directory' .
bwoDetachObject :: Lens' BatchWriteOperationResponse (Maybe BatchDetachObjectResponse)
bwoDetachObject = lens _bwoDetachObject (\ s a -> s{_bwoDetachObject = a});

-- | Result of an add facet to object batch operation.
bwoAddFacetToObject :: Lens' BatchWriteOperationResponse (Maybe BatchAddFacetToObjectResponse)
bwoAddFacetToObject = lens _bwoAddFacetToObject (\ s a -> s{_bwoAddFacetToObject = a});

-- | Updates a given object’s attributes.
bwoUpdateObjectAttributes :: Lens' BatchWriteOperationResponse (Maybe BatchUpdateObjectAttributesResponse)
bwoUpdateObjectAttributes = lens _bwoUpdateObjectAttributes (\ s a -> s{_bwoUpdateObjectAttributes = a});

instance FromJSON BatchWriteOperationResponse where
        parseJSON
          = withObject "BatchWriteOperationResponse"
              (\ x ->
                 BatchWriteOperationResponse' <$>
                   (x .:? "DeleteObject") <*>
                     (x .:? "RemoveFacetFromObject")
                     <*> (x .:? "AttachObject")
                     <*> (x .:? "CreateObject")
                     <*> (x .:? "DetachObject")
                     <*> (x .:? "AddFacetToObject")
                     <*> (x .:? "UpdateObjectAttributes"))

instance Hashable BatchWriteOperationResponse

instance NFData BatchWriteOperationResponse

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Directory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDirectoryARN' - ARN associated with the directory. For more information, see 'arns' .
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

-- | ARN associated with the directory. For more information, see 'arns' .
dDirectoryARN :: Lens' Directory (Maybe Text)
dDirectoryARN = lens _dDirectoryARN (\ s a -> s{_dDirectoryARN = a});

-- | The state of the directory. Can be either @Enabled@ , @Disabled@ , or @Deleted@ .
dState :: Lens' Directory (Maybe DirectoryState)
dState = lens _dState (\ s a -> s{_dState = a});

-- | The name of the directory.
dName :: Lens' Directory (Maybe Text)
dName = lens _dName (\ s a -> s{_dName = a});

-- | The date and time when the directory was created.
dCreationDateTime :: Lens' Directory (Maybe UTCTime)
dCreationDateTime = lens _dCreationDateTime (\ s a -> s{_dCreationDateTime = a}) . mapping _Time;

instance FromJSON Directory where
        parseJSON
          = withObject "Directory"
              (\ x ->
                 Directory' <$>
                   (x .:? "DirectoryArn") <*> (x .:? "State") <*>
                     (x .:? "Name")
                     <*> (x .:? "CreationDateTime"))

instance Hashable Directory

instance NFData Directory

-- | A structure that contains @Name@ , @ARN@ , @Attributes@ , 'Rule' s, and @ObjectTypes@ .
--
--
--
-- /See:/ 'facet' smart constructor.
data Facet = Facet'
    { _fObjectType :: !(Maybe ObjectType)
    , _fName       :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Facet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fObjectType' - Object type associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
--
-- * 'fName' - The name of the 'Facet' .
facet
    :: Facet
facet =
    Facet'
    { _fObjectType = Nothing
    , _fName = Nothing
    }

-- | Object type associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
fObjectType :: Lens' Facet (Maybe ObjectType)
fObjectType = lens _fObjectType (\ s a -> s{_fObjectType = a});

-- | The name of the 'Facet' .
fName :: Lens' Facet (Maybe Text)
fName = lens _fName (\ s a -> s{_fName = a});

instance FromJSON Facet where
        parseJSON
          = withObject "Facet"
              (\ x ->
                 Facet' <$> (x .:? "ObjectType") <*> (x .:? "Name"))

instance Hashable Facet

instance NFData Facet

-- | Attribute associated with the 'Facet' .
--
--
--
-- /See:/ 'facetAttribute' smart constructor.
data FacetAttribute = FacetAttribute'
    { _faAttributeReference  :: !(Maybe FacetAttributeReference)
    , _faAttributeDefinition :: !(Maybe FacetAttributeDefinition)
    , _faRequiredBehavior    :: !(Maybe RequiredAttributeBehavior)
    , _faName                :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FacetAttribute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'faAttributeReference' - Attribute reference associated with the attribute. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
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

-- | Attribute reference associated with the attribute. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
faAttributeReference :: Lens' FacetAttribute (Maybe FacetAttributeReference)
faAttributeReference = lens _faAttributeReference (\ s a -> s{_faAttributeReference = a});

-- | A facet attribute consists of either a definition or a reference. This structure contains the attribute definition. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
faAttributeDefinition :: Lens' FacetAttribute (Maybe FacetAttributeDefinition)
faAttributeDefinition = lens _faAttributeDefinition (\ s a -> s{_faAttributeDefinition = a});

-- | The required behavior of the @FacetAttribute@ .
faRequiredBehavior :: Lens' FacetAttribute (Maybe RequiredAttributeBehavior)
faRequiredBehavior = lens _faRequiredBehavior (\ s a -> s{_faRequiredBehavior = a});

-- | The name of the facet attribute.
faName :: Lens' FacetAttribute Text
faName = lens _faName (\ s a -> s{_faName = a});

instance FromJSON FacetAttribute where
        parseJSON
          = withObject "FacetAttribute"
              (\ x ->
                 FacetAttribute' <$>
                   (x .:? "AttributeReference") <*>
                     (x .:? "AttributeDefinition")
                     <*> (x .:? "RequiredBehavior")
                     <*> (x .: "Name"))

instance Hashable FacetAttribute

instance NFData FacetAttribute

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
fadRules = lens _fadRules (\ s a -> s{_fadRules = a}) . _Default . _Map;

-- | The default value of the attribute (if configured).
fadDefaultValue :: Lens' FacetAttributeDefinition (Maybe TypedAttributeValue)
fadDefaultValue = lens _fadDefaultValue (\ s a -> s{_fadDefaultValue = a});

-- | Whether the attribute is mutable or not.
fadIsImmutable :: Lens' FacetAttributeDefinition (Maybe Bool)
fadIsImmutable = lens _fadIsImmutable (\ s a -> s{_fadIsImmutable = a});

-- | The type of the attribute.
fadType :: Lens' FacetAttributeDefinition FacetAttributeType
fadType = lens _fadType (\ s a -> s{_fadType = a});

instance FromJSON FacetAttributeDefinition where
        parseJSON
          = withObject "FacetAttributeDefinition"
              (\ x ->
                 FacetAttributeDefinition' <$>
                   (x .:? "Rules" .!= mempty) <*> (x .:? "DefaultValue")
                     <*> (x .:? "IsImmutable")
                     <*> (x .: "Type"))

instance Hashable FacetAttributeDefinition

instance NFData FacetAttributeDefinition

instance ToJSON FacetAttributeDefinition where
        toJSON FacetAttributeDefinition'{..}
          = object
              (catMaybes
                 [("Rules" .=) <$> _fadRules,
                  ("DefaultValue" .=) <$> _fadDefaultValue,
                  ("IsImmutable" .=) <$> _fadIsImmutable,
                  Just ("Type" .= _fadType)])

-- | Facet attribute reference that specifies the attribute definition which contains attribute facet name and attribute name.
--
--
--
-- /See:/ 'facetAttributeReference' smart constructor.
data FacetAttributeReference = FacetAttributeReference'
    { _farTargetFacetName     :: !Text
    , _farTargetAttributeName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FacetAttributeReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'farTargetFacetName' - Target facet name associated with the facet reference. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
--
-- * 'farTargetAttributeName' - Target attribute name associated with the facet reference. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
facetAttributeReference
    :: Text -- ^ 'farTargetFacetName'
    -> Text -- ^ 'farTargetAttributeName'
    -> FacetAttributeReference
facetAttributeReference pTargetFacetName_ pTargetAttributeName_ =
    FacetAttributeReference'
    { _farTargetFacetName = pTargetFacetName_
    , _farTargetAttributeName = pTargetAttributeName_
    }

-- | Target facet name associated with the facet reference. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
farTargetFacetName :: Lens' FacetAttributeReference Text
farTargetFacetName = lens _farTargetFacetName (\ s a -> s{_farTargetFacetName = a});

-- | Target attribute name associated with the facet reference. See <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_advanced.html#attributereferences Attribute References> for more information.
farTargetAttributeName :: Lens' FacetAttributeReference Text
farTargetAttributeName = lens _farTargetAttributeName (\ s a -> s{_farTargetAttributeName = a});

instance FromJSON FacetAttributeReference where
        parseJSON
          = withObject "FacetAttributeReference"
              (\ x ->
                 FacetAttributeReference' <$>
                   (x .: "TargetFacetName") <*>
                     (x .: "TargetAttributeName"))

instance Hashable FacetAttributeReference

instance NFData FacetAttributeReference

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    FacetAttributeUpdate'
    { _fauAttribute = Nothing
    , _fauAction = Nothing
    }

-- | The attribute to update.
fauAttribute :: Lens' FacetAttributeUpdate (Maybe FacetAttribute)
fauAttribute = lens _fauAttribute (\ s a -> s{_fauAttribute = a});

-- | The action to perform when updating the attribute.
fauAction :: Lens' FacetAttributeUpdate (Maybe UpdateActionType)
fauAction = lens _fauAction (\ s a -> s{_fauAction = a});

instance Hashable FacetAttributeUpdate

instance NFData FacetAttributeUpdate

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IndexAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaIndexedAttributes' - The indexed attribute values.
--
-- * 'iaObjectIdentifier' - The @ObjectIdentifier@ of the object attached to the index.
indexAttachment
    :: IndexAttachment
indexAttachment =
    IndexAttachment'
    { _iaIndexedAttributes = Nothing
    , _iaObjectIdentifier = Nothing
    }

-- | The indexed attribute values.
iaIndexedAttributes :: Lens' IndexAttachment [AttributeKeyAndValue]
iaIndexedAttributes = lens _iaIndexedAttributes (\ s a -> s{_iaIndexedAttributes = a}) . _Default . _Coerce;

-- | The @ObjectIdentifier@ of the object attached to the index.
iaObjectIdentifier :: Lens' IndexAttachment (Maybe Text)
iaObjectIdentifier = lens _iaObjectIdentifier (\ s a -> s{_iaObjectIdentifier = a});

instance FromJSON IndexAttachment where
        parseJSON
          = withObject "IndexAttachment"
              (\ x ->
                 IndexAttachment' <$>
                   (x .:? "IndexedAttributes" .!= mempty) <*>
                     (x .:? "ObjectIdentifier"))

instance Hashable IndexAttachment

instance NFData IndexAttachment

-- | The action to take on the object attribute.
--
--
--
-- /See:/ 'objectAttributeAction' smart constructor.
data ObjectAttributeAction = ObjectAttributeAction'
    { _oaaObjectAttributeActionType  :: !(Maybe UpdateActionType)
    , _oaaObjectAttributeUpdateValue :: !(Maybe TypedAttributeValue)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ObjectAttributeAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oaaObjectAttributeActionType' - Type can be either Update or Delete.
--
-- * 'oaaObjectAttributeUpdateValue' - The value that you want to update to.
objectAttributeAction
    :: ObjectAttributeAction
objectAttributeAction =
    ObjectAttributeAction'
    { _oaaObjectAttributeActionType = Nothing
    , _oaaObjectAttributeUpdateValue = Nothing
    }

-- | Type can be either Update or Delete.
oaaObjectAttributeActionType :: Lens' ObjectAttributeAction (Maybe UpdateActionType)
oaaObjectAttributeActionType = lens _oaaObjectAttributeActionType (\ s a -> s{_oaaObjectAttributeActionType = a});

-- | The value that you want to update to.
oaaObjectAttributeUpdateValue :: Lens' ObjectAttributeAction (Maybe TypedAttributeValue)
oaaObjectAttributeUpdateValue = lens _oaaObjectAttributeUpdateValue (\ s a -> s{_oaaObjectAttributeUpdateValue = a});

instance Hashable ObjectAttributeAction

instance NFData ObjectAttributeAction

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ObjectAttributeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oarRange' - The range of attribute values being selected.
--
-- * 'oarAttributeKey' - The key of the attribute the attribute range covers.
objectAttributeRange
    :: ObjectAttributeRange
objectAttributeRange =
    ObjectAttributeRange'
    { _oarRange = Nothing
    , _oarAttributeKey = Nothing
    }

-- | The range of attribute values being selected.
oarRange :: Lens' ObjectAttributeRange (Maybe TypedAttributeValueRange)
oarRange = lens _oarRange (\ s a -> s{_oarRange = a});

-- | The key of the attribute the attribute range covers.
oarAttributeKey :: Lens' ObjectAttributeRange (Maybe AttributeKey)
oarAttributeKey = lens _oarAttributeKey (\ s a -> s{_oarAttributeKey = a});

instance Hashable ObjectAttributeRange

instance NFData ObjectAttributeRange

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
    { _oauObjectAttributeAction = Nothing
    , _oauObjectAttributeKey = Nothing
    }

-- | The action to perform as part of the attribute update.
oauObjectAttributeAction :: Lens' ObjectAttributeUpdate (Maybe ObjectAttributeAction)
oauObjectAttributeAction = lens _oauObjectAttributeAction (\ s a -> s{_oauObjectAttributeAction = a});

-- | The key of the attribute being updated.
oauObjectAttributeKey :: Lens' ObjectAttributeUpdate (Maybe AttributeKey)
oauObjectAttributeKey = lens _oauObjectAttributeKey (\ s a -> s{_oauObjectAttributeKey = a});

instance Hashable ObjectAttributeUpdate

instance NFData ObjectAttributeUpdate

instance ToJSON ObjectAttributeUpdate where
        toJSON ObjectAttributeUpdate'{..}
          = object
              (catMaybes
                 [("ObjectAttributeAction" .=) <$>
                    _oauObjectAttributeAction,
                  ("ObjectAttributeKey" .=) <$>
                    _oauObjectAttributeKey])

-- | Reference that identifies an object.
--
--
--
-- /See:/ 'objectReference' smart constructor.
newtype ObjectReference = ObjectReference'
    { _orSelector :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ObjectReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'orSelector' - Allows you to specify an object. You can identify an object in one of the following ways:     * /> ObjectIdentifier/ - Identifies the object by @ObjectIdentifier@      * /\/some\/path/ - Identifies the object based on path     * /#SomeBatchReference/ - Identifies the object in a batch call
objectReference
    :: ObjectReference
objectReference =
    ObjectReference'
    { _orSelector = Nothing
    }

-- | Allows you to specify an object. You can identify an object in one of the following ways:     * /> ObjectIdentifier/ - Identifies the object by @ObjectIdentifier@      * /\/some\/path/ - Identifies the object based on path     * /#SomeBatchReference/ - Identifies the object in a batch call
orSelector :: Lens' ObjectReference (Maybe Text)
orSelector = lens _orSelector (\ s a -> s{_orSelector = a});

instance Hashable ObjectReference

instance NFData ObjectReference

instance ToJSON ObjectReference where
        toJSON ObjectReference'{..}
          = object
              (catMaybes [("Selector" .=) <$> _orSelector])

-- | Returns the path to the @ObjectIdentifiers@ associated with the directory.
--
--
--
-- /See:/ 'pathToObjectIdentifiers' smart constructor.
data PathToObjectIdentifiers = PathToObjectIdentifiers'
    { _ptoiObjectIdentifiers :: !(Maybe [Text])
    , _ptoiPath              :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PathToObjectIdentifiers' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptoiObjectIdentifiers' - Lists @ObjectIdentifiers@ starting from directory root to the object in the request.
--
-- * 'ptoiPath' - The path used to identify the object starting from directory root.
pathToObjectIdentifiers
    :: PathToObjectIdentifiers
pathToObjectIdentifiers =
    PathToObjectIdentifiers'
    { _ptoiObjectIdentifiers = Nothing
    , _ptoiPath = Nothing
    }

-- | Lists @ObjectIdentifiers@ starting from directory root to the object in the request.
ptoiObjectIdentifiers :: Lens' PathToObjectIdentifiers [Text]
ptoiObjectIdentifiers = lens _ptoiObjectIdentifiers (\ s a -> s{_ptoiObjectIdentifiers = a}) . _Default . _Coerce;

-- | The path used to identify the object starting from directory root.
ptoiPath :: Lens' PathToObjectIdentifiers (Maybe Text)
ptoiPath = lens _ptoiPath (\ s a -> s{_ptoiPath = a});

instance FromJSON PathToObjectIdentifiers where
        parseJSON
          = withObject "PathToObjectIdentifiers"
              (\ x ->
                 PathToObjectIdentifiers' <$>
                   (x .:? "ObjectIdentifiers" .!= mempty) <*>
                     (x .:? "Path"))

instance Hashable PathToObjectIdentifiers

instance NFData PathToObjectIdentifiers

-- | Contains the @PolicyType@ , @PolicyId@ , and the @ObjectIdentifier@ to which it is attached.
--
--
--
-- /See:/ 'policyAttachment' smart constructor.
data PolicyAttachment = PolicyAttachment'
    { _paPolicyId         :: !(Maybe Text)
    , _paPolicyType       :: !(Maybe Text)
    , _paObjectIdentifier :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PolicyAttachment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paPolicyId' - The ID of @PolicyAttachment@ .
--
-- * 'paPolicyType' - The type of policy that can be associated with @PolicyAttachment@ .
--
-- * 'paObjectIdentifier' - The @ObjectIdentifier@ associated with @PolicyAttachment@ .
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
paPolicyId = lens _paPolicyId (\ s a -> s{_paPolicyId = a});

-- | The type of policy that can be associated with @PolicyAttachment@ .
paPolicyType :: Lens' PolicyAttachment (Maybe Text)
paPolicyType = lens _paPolicyType (\ s a -> s{_paPolicyType = a});

-- | The @ObjectIdentifier@ associated with @PolicyAttachment@ .
paObjectIdentifier :: Lens' PolicyAttachment (Maybe Text)
paObjectIdentifier = lens _paObjectIdentifier (\ s a -> s{_paObjectIdentifier = a});

instance FromJSON PolicyAttachment where
        parseJSON
          = withObject "PolicyAttachment"
              (\ x ->
                 PolicyAttachment' <$>
                   (x .:? "PolicyId") <*> (x .:? "PolicyType") <*>
                     (x .:? "ObjectIdentifier"))

instance Hashable PolicyAttachment

instance NFData PolicyAttachment

-- | Used when a regular object exists in a 'Directory' and you want to find all of the policies associated with that object and the parent to that object.
--
--
--
-- /See:/ 'policyToPath' smart constructor.
data PolicyToPath = PolicyToPath'
    { _ptpPath     :: !(Maybe Text)
    , _ptpPolicies :: !(Maybe [PolicyAttachment])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PolicyToPath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ptpPath' - The path that is referenced from the root.
--
-- * 'ptpPolicies' - List of policy objects.
policyToPath
    :: PolicyToPath
policyToPath =
    PolicyToPath'
    { _ptpPath = Nothing
    , _ptpPolicies = Nothing
    }

-- | The path that is referenced from the root.
ptpPath :: Lens' PolicyToPath (Maybe Text)
ptpPath = lens _ptpPath (\ s a -> s{_ptpPath = a});

-- | List of policy objects.
ptpPolicies :: Lens' PolicyToPath [PolicyAttachment]
ptpPolicies = lens _ptpPolicies (\ s a -> s{_ptpPolicies = a}) . _Default . _Coerce;

instance FromJSON PolicyToPath where
        parseJSON
          = withObject "PolicyToPath"
              (\ x ->
                 PolicyToPath' <$>
                   (x .:? "Path") <*> (x .:? "Policies" .!= mempty))

instance Hashable PolicyToPath

instance NFData PolicyToPath

-- | Contains an ARN and parameters associated with the rule.
--
--
--
-- /See:/ 'rule' smart constructor.
data Rule = Rule'
    { _rParameters :: !(Maybe (Map Text Text))
    , _rType       :: !(Maybe RuleType)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rParameters' - Min and max parameters associated with the rule.
--
-- * 'rType' - The type of attribute validation rule.
rule
    :: Rule
rule =
    Rule'
    { _rParameters = Nothing
    , _rType = Nothing
    }

-- | Min and max parameters associated with the rule.
rParameters :: Lens' Rule (HashMap Text Text)
rParameters = lens _rParameters (\ s a -> s{_rParameters = a}) . _Default . _Map;

-- | The type of attribute validation rule.
rType :: Lens' Rule (Maybe RuleType)
rType = lens _rType (\ s a -> s{_rType = a});

instance FromJSON Rule where
        parseJSON
          = withObject "Rule"
              (\ x ->
                 Rule' <$>
                   (x .:? "Parameters" .!= mempty) <*> (x .:? "Type"))

instance Hashable Rule

instance NFData Rule

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SchemaFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfFacetName' - The name of the facet.
--
-- * 'sfSchemaARN' - The ARN of the schema that contains the facet.
schemaFacet
    :: SchemaFacet
schemaFacet =
    SchemaFacet'
    { _sfFacetName = Nothing
    , _sfSchemaARN = Nothing
    }

-- | The name of the facet.
sfFacetName :: Lens' SchemaFacet (Maybe Text)
sfFacetName = lens _sfFacetName (\ s a -> s{_sfFacetName = a});

-- | The ARN of the schema that contains the facet.
sfSchemaARN :: Lens' SchemaFacet (Maybe Text)
sfSchemaARN = lens _sfSchemaARN (\ s a -> s{_sfSchemaARN = a});

instance FromJSON SchemaFacet where
        parseJSON
          = withObject "SchemaFacet"
              (\ x ->
                 SchemaFacet' <$>
                   (x .:? "FacetName") <*> (x .:? "SchemaArn"))

instance Hashable SchemaFacet

instance NFData SchemaFacet

instance ToJSON SchemaFacet where
        toJSON SchemaFacet'{..}
          = object
              (catMaybes
                 [("FacetName" .=) <$> _sfFacetName,
                  ("SchemaArn" .=) <$> _sfSchemaARN])

-- | Tag structure which contains tag key and value.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - Value associated with the tag.
--
-- * 'tagKey' - Key associated with the tag.
tag
    :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | Value associated with the tag.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | Key associated with the tag.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag

instance NFData Tag

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
tavBinaryValue = lens _tavBinaryValue (\ s a -> s{_tavBinaryValue = a}) . mapping _Base64;

-- | A date and time value.
tavDatetimeValue :: Lens' TypedAttributeValue (Maybe UTCTime)
tavDatetimeValue = lens _tavDatetimeValue (\ s a -> s{_tavDatetimeValue = a}) . mapping _Time;

-- | A number data value.
tavNumberValue :: Lens' TypedAttributeValue (Maybe Text)
tavNumberValue = lens _tavNumberValue (\ s a -> s{_tavNumberValue = a});

-- | A string data value.
tavStringValue :: Lens' TypedAttributeValue (Maybe Text)
tavStringValue = lens _tavStringValue (\ s a -> s{_tavStringValue = a});

-- | A Boolean data value.
tavBooleanValue :: Lens' TypedAttributeValue (Maybe Bool)
tavBooleanValue = lens _tavBooleanValue (\ s a -> s{_tavBooleanValue = a});

instance FromJSON TypedAttributeValue where
        parseJSON
          = withObject "TypedAttributeValue"
              (\ x ->
                 TypedAttributeValue' <$>
                   (x .:? "BinaryValue") <*> (x .:? "DatetimeValue") <*>
                     (x .:? "NumberValue")
                     <*> (x .:? "StringValue")
                     <*> (x .:? "BooleanValue"))

instance Hashable TypedAttributeValue

instance NFData TypedAttributeValue

instance ToJSON TypedAttributeValue where
        toJSON TypedAttributeValue'{..}
          = object
              (catMaybes
                 [("BinaryValue" .=) <$> _tavBinaryValue,
                  ("DatetimeValue" .=) <$> _tavDatetimeValue,
                  ("NumberValue" .=) <$> _tavNumberValue,
                  ("StringValue" .=) <$> _tavStringValue,
                  ("BooleanValue" .=) <$> _tavBooleanValue])

-- | A range of attribute values.
--
--
--
-- /See:/ 'typedAttributeValueRange' smart constructor.
data TypedAttributeValueRange = TypedAttributeValueRange'
    { _tavrEndValue   :: !(Maybe TypedAttributeValue)
    , _tavrStartValue :: !(Maybe TypedAttributeValue)
    , _tavrStartMode  :: !RangeMode
    , _tavrEndMode    :: !RangeMode
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TypedAttributeValueRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tavrEndValue' - The attribute value to terminate the range at.
--
-- * 'tavrStartValue' - The value to start the range at.
--
-- * 'tavrStartMode' - Inclusive or exclusive range start.
--
-- * 'tavrEndMode' - Inclusive or exclusive range end.
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
tavrEndValue = lens _tavrEndValue (\ s a -> s{_tavrEndValue = a});

-- | The value to start the range at.
tavrStartValue :: Lens' TypedAttributeValueRange (Maybe TypedAttributeValue)
tavrStartValue = lens _tavrStartValue (\ s a -> s{_tavrStartValue = a});

-- | Inclusive or exclusive range start.
tavrStartMode :: Lens' TypedAttributeValueRange RangeMode
tavrStartMode = lens _tavrStartMode (\ s a -> s{_tavrStartMode = a});

-- | Inclusive or exclusive range end.
tavrEndMode :: Lens' TypedAttributeValueRange RangeMode
tavrEndMode = lens _tavrEndMode (\ s a -> s{_tavrEndMode = a});

instance Hashable TypedAttributeValueRange

instance NFData TypedAttributeValueRange

instance ToJSON TypedAttributeValueRange where
        toJSON TypedAttributeValueRange'{..}
          = object
              (catMaybes
                 [("EndValue" .=) <$> _tavrEndValue,
                  ("StartValue" .=) <$> _tavrStartValue,
                  Just ("StartMode" .= _tavrStartMode),
                  Just ("EndMode" .= _tavrEndMode)])
