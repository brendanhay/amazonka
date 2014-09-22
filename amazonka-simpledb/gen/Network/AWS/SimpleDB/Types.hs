{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon SimpleDB is a highly available and flexible non-relational data
-- store that offloads the work of database administration. Developers simply
-- store and query data items via web services requests and Amazon SimpleDB
-- does the rest. Unbound by the strict requirements of a relational database,
-- Amazon SimpleDB is optimized to provide high availability and flexibility,
-- with little or no administrative burden. Behind the scenes, Amazon SimpleDB
-- creates and manages multiple geographically distributed replicas of your
-- data automatically to enable high availability and data durability. The
-- service charges you only for the resources actually consumed in storing
-- your data and serving your requests. You can change your data model on the
-- fly, and data is automatically indexed for you. With Amazon SimpleDB, you
-- can focus on application development without worrying about infrastructure
-- provisioning, high availability, software maintenance, schema and index
-- management, or performance tuning.
module Network.AWS.SimpleDB.Types
    (
    -- * Service
      SimpleDB
    -- ** Errors
    , SimpleDBError (..)
    , _AttributeDoesNotExist
    , _DuplicateItemName
    , _InvalidNextToken
    , _InvalidNumberPredicates
    , _InvalidNumberValueTests
    , _InvalidParameterValue
    , _InvalidQueryExpression
    , _MissingParameter
    , _NoSuchDomain
    , _NumberDomainAttributesExceeded
    , _NumberDomainBytesExceeded
    , _NumberDomainsExceeded
    , _NumberItemAttributesExceeded
    , _NumberSubmittedAttributesExceeded
    , _NumberSubmittedItemsExceeded
    , _RequestTimeout
    , _SimpleDBClient
    , _SimpleDBSerializer
    , _SimpleDBService
    , _TooManyRequestedAttributes
    -- ** XML
    , xmlOptions

    -- * Attribute
    , Attribute
    , attribute
    , aName
    , aAlternateNameEncoding
    , aValue
    , aAlternateValueEncoding

    -- * DeletableItem
    , DeletableItem
    , deletableItem
    , diName
    , diAttributes

    -- * Item
    , Item
    , item
    , iName
    , iAlternateNameEncoding
    , iAttributes

    -- * ReplaceableAttribute
    , ReplaceableAttribute
    , replaceableAttribute
    , raName
    , raValue
    , raReplace

    -- * ReplaceableItem
    , ReplaceableItem
    , replaceableItem
    , riName
    , riAttributes

    -- * UpdateCondition
    , UpdateCondition
    , updateCondition
    , ucName
    , ucValue
    , ucExists
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V2

-- | Supported version (@2009-04-15@) of the
-- @Amazon SimpleDB@ service.
data SimpleDB deriving (Typeable)

instance AWSService SimpleDB where
    type Sg SimpleDB = V2
    type Er SimpleDB = SimpleDBError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "sdb"
        , _svcVersion  = "2009-04-15"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'SimpleDB' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data SimpleDBError
      -- | The specified attribute does not exist.
    = AttributeDoesNotExist
        { _adneBoxUsage :: Maybe Double
        }
      -- | The item name was specified more than once.
    | DuplicateItemName
        { _dinBoxUsage :: Maybe Double
        }
      -- | The specified NextToken is not valid.
    | InvalidNextToken
        { _intBoxUsage :: Maybe Double
        }
      -- | Too many predicates exist in the query expression.
    | InvalidNumberPredicates
        { _inpBoxUsage :: Maybe Double
        }
      -- | Too many predicates exist in the query expression.
    | InvalidNumberValueTests
        { _invtBoxUsage :: Maybe Double
        }
      -- | The value for a parameter is invalid.
    | InvalidParameterValue
        { _ipvBoxUsage :: Maybe Double
        }
      -- | The specified query expression syntax is not valid.
    | InvalidQueryExpression
        { _iqeBoxUsage :: Maybe Double
        }
      -- | The request must contain the specified missing parameter.
    | MissingParameter
        { _mpBoxUsage :: Maybe Double
        }
      -- | The specified domain does not exist.
    | NoSuchDomain
        { _nsdBoxUsage :: Maybe Double
        }
      -- | Too many attributes in this domain.
    | NumberDomainAttributesExceeded
        { _ndaeBoxUsage :: Maybe Double
        }
      -- | Too many bytes in this domain.
    | NumberDomainBytesExceeded
        { _ndbeBoxUsage :: Maybe Double
        }
      -- | Too many domains exist per this account.
    | NumberDomainsExceeded
        { _ndeBoxUsage :: Maybe Double
        }
      -- | Too many attributes in this item.
    | NumberItemAttributesExceeded
        { _niaeBoxUsage :: Maybe Double
        }
      -- | Too many attributes exist in a single call.
    | NumberSubmittedAttributesExceeded
        { _nsaeBoxUsage :: Maybe Double
        }
      -- | Too many items exist in a single call.
    | NumberSubmittedItemsExceeded
        { _nsieBoxUsage :: Maybe Double
        }
      -- | A timeout occurred when attempting to query the specified domain
      -- with specified query expression.
    | RequestTimeout
        { _rtBoxUsage :: Maybe Double
        }
    | SimpleDBClient HttpException
    | SimpleDBSerializer String
    | SimpleDBService String
      -- | Too many attributes requested.
    | TooManyRequestedAttributes
        { _tmraBoxUsage :: Maybe Double
        }
      deriving (Show, Typeable, Generic)

instance AWSError SimpleDBError where
    awsError = const "SimpleDBError"

instance AWSServiceError SimpleDBError where
    serviceError    = SimpleDBService
    clientError     = SimpleDBClient
    serializerError = SimpleDBSerializer

instance Exception SimpleDBError

-- | The specified attribute does not exist.
--
-- See: 'AttributeDoesNotExist'
_AttributeDoesNotExist :: Prism' SimpleDBError (Maybe Double)
_AttributeDoesNotExist = prism
    AttributeDoesNotExist
    (\case
        AttributeDoesNotExist p1 -> Right p1
        x -> Left x)

-- | The item name was specified more than once.
--
-- See: 'DuplicateItemName'
_DuplicateItemName :: Prism' SimpleDBError (Maybe Double)
_DuplicateItemName = prism
    DuplicateItemName
    (\case
        DuplicateItemName p1 -> Right p1
        x -> Left x)

-- | The specified NextToken is not valid.
--
-- See: 'InvalidNextToken'
_InvalidNextToken :: Prism' SimpleDBError (Maybe Double)
_InvalidNextToken = prism
    InvalidNextToken
    (\case
        InvalidNextToken p1 -> Right p1
        x -> Left x)

-- | Too many predicates exist in the query expression.
--
-- See: 'InvalidNumberPredicates'
_InvalidNumberPredicates :: Prism' SimpleDBError (Maybe Double)
_InvalidNumberPredicates = prism
    InvalidNumberPredicates
    (\case
        InvalidNumberPredicates p1 -> Right p1
        x -> Left x)

-- | Too many predicates exist in the query expression.
--
-- See: 'InvalidNumberValueTests'
_InvalidNumberValueTests :: Prism' SimpleDBError (Maybe Double)
_InvalidNumberValueTests = prism
    InvalidNumberValueTests
    (\case
        InvalidNumberValueTests p1 -> Right p1
        x -> Left x)

-- | The value for a parameter is invalid.
--
-- See: 'InvalidParameterValue'
_InvalidParameterValue :: Prism' SimpleDBError (Maybe Double)
_InvalidParameterValue = prism
    InvalidParameterValue
    (\case
        InvalidParameterValue p1 -> Right p1
        x -> Left x)

-- | The specified query expression syntax is not valid.
--
-- See: 'InvalidQueryExpression'
_InvalidQueryExpression :: Prism' SimpleDBError (Maybe Double)
_InvalidQueryExpression = prism
    InvalidQueryExpression
    (\case
        InvalidQueryExpression p1 -> Right p1
        x -> Left x)

-- | The request must contain the specified missing parameter.
--
-- See: 'MissingParameter'
_MissingParameter :: Prism' SimpleDBError (Maybe Double)
_MissingParameter = prism
    MissingParameter
    (\case
        MissingParameter p1 -> Right p1
        x -> Left x)

-- | The specified domain does not exist.
--
-- See: 'NoSuchDomain'
_NoSuchDomain :: Prism' SimpleDBError (Maybe Double)
_NoSuchDomain = prism
    NoSuchDomain
    (\case
        NoSuchDomain p1 -> Right p1
        x -> Left x)

-- | Too many attributes in this domain.
--
-- See: 'NumberDomainAttributesExceeded'
_NumberDomainAttributesExceeded :: Prism' SimpleDBError (Maybe Double)
_NumberDomainAttributesExceeded = prism
    NumberDomainAttributesExceeded
    (\case
        NumberDomainAttributesExceeded p1 -> Right p1
        x -> Left x)

-- | Too many bytes in this domain.
--
-- See: 'NumberDomainBytesExceeded'
_NumberDomainBytesExceeded :: Prism' SimpleDBError (Maybe Double)
_NumberDomainBytesExceeded = prism
    NumberDomainBytesExceeded
    (\case
        NumberDomainBytesExceeded p1 -> Right p1
        x -> Left x)

-- | Too many domains exist per this account.
--
-- See: 'NumberDomainsExceeded'
_NumberDomainsExceeded :: Prism' SimpleDBError (Maybe Double)
_NumberDomainsExceeded = prism
    NumberDomainsExceeded
    (\case
        NumberDomainsExceeded p1 -> Right p1
        x -> Left x)

-- | Too many attributes in this item.
--
-- See: 'NumberItemAttributesExceeded'
_NumberItemAttributesExceeded :: Prism' SimpleDBError (Maybe Double)
_NumberItemAttributesExceeded = prism
    NumberItemAttributesExceeded
    (\case
        NumberItemAttributesExceeded p1 -> Right p1
        x -> Left x)

-- | Too many attributes exist in a single call.
--
-- See: 'NumberSubmittedAttributesExceeded'
_NumberSubmittedAttributesExceeded :: Prism' SimpleDBError (Maybe Double)
_NumberSubmittedAttributesExceeded = prism
    NumberSubmittedAttributesExceeded
    (\case
        NumberSubmittedAttributesExceeded p1 -> Right p1
        x -> Left x)

-- | Too many items exist in a single call.
--
-- See: 'NumberSubmittedItemsExceeded'
_NumberSubmittedItemsExceeded :: Prism' SimpleDBError (Maybe Double)
_NumberSubmittedItemsExceeded = prism
    NumberSubmittedItemsExceeded
    (\case
        NumberSubmittedItemsExceeded p1 -> Right p1
        x -> Left x)

-- | A timeout occurred when attempting to query the specified domain with
-- specified query expression.
--
-- See: 'RequestTimeout'
_RequestTimeout :: Prism' SimpleDBError (Maybe Double)
_RequestTimeout = prism
    RequestTimeout
    (\case
        RequestTimeout p1 -> Right p1
        x -> Left x)

-- | See: 'SimpleDBClient'
_SimpleDBClient :: Prism' SimpleDBError HttpException
_SimpleDBClient = prism
    SimpleDBClient
    (\case
        SimpleDBClient p1 -> Right p1
        x -> Left x)

-- | See: 'SimpleDBSerializer'
_SimpleDBSerializer :: Prism' SimpleDBError String
_SimpleDBSerializer = prism
    SimpleDBSerializer
    (\case
        SimpleDBSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'SimpleDBService'
_SimpleDBService :: Prism' SimpleDBError String
_SimpleDBService = prism
    SimpleDBService
    (\case
        SimpleDBService p1 -> Right p1
        x -> Left x)

-- | Too many attributes requested.
--
-- See: 'TooManyRequestedAttributes'
_TooManyRequestedAttributes :: Prism' SimpleDBError (Maybe Double)
_TooManyRequestedAttributes = prism
    TooManyRequestedAttributes
    (\case
        TooManyRequestedAttributes p1 -> Right p1
        x -> Left x)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def

-- | 
data Attribute = Attribute
    { _aName :: Text
    , _aAlternateNameEncoding :: Maybe Text
    , _aValue :: Text
    , _aAlternateValueEncoding :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Attribute' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @AlternateNameEncoding ::@ @Maybe Text@
--
-- * @Value ::@ @Text@
--
-- * @AlternateValueEncoding ::@ @Maybe Text@
--
attribute :: Text -- ^ 'aName'
          -> Text -- ^ 'aValue'
          -> Attribute
attribute p1 p3 = Attribute
    { _aName = p1
    , _aAlternateNameEncoding = Nothing
    , _aValue = p3
    , _aAlternateValueEncoding = Nothing
    }

-- | The name of the attribute.
aName :: Lens' Attribute Text
aName = lens _aName (\s a -> s { _aName = a })

-- | 
aAlternateNameEncoding :: Lens' Attribute (Maybe Text)
aAlternateNameEncoding =
    lens _aAlternateNameEncoding (\s a -> s { _aAlternateNameEncoding = a })

-- | The value of the attribute.
aValue :: Lens' Attribute Text
aValue = lens _aValue (\s a -> s { _aValue = a })

-- | 
aAlternateValueEncoding :: Lens' Attribute (Maybe Text)
aAlternateValueEncoding =
    lens _aAlternateValueEncoding
         (\s a -> s { _aAlternateValueEncoding = a })

instance FromXML Attribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Attribute"

instance ToQuery Attribute where
    toQuery = genericQuery def

data DeletableItem = DeletableItem
    { _diName :: Text
    , _diAttributes :: [Attribute]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeletableItem' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @Attributes ::@ @[Attribute]@
--
deletableItem :: Text -- ^ 'diName'
              -> DeletableItem
deletableItem p1 = DeletableItem
    { _diName = p1
    , _diAttributes = mempty
    }

diName :: Lens' DeletableItem Text
diName = lens _diName (\s a -> s { _diName = a })

diAttributes :: Lens' DeletableItem [Attribute]
diAttributes = lens _diAttributes (\s a -> s { _diAttributes = a })

instance ToQuery DeletableItem where
    toQuery = genericQuery def

-- | 
data Item = Item
    { _iName :: Text
    , _iAlternateNameEncoding :: Maybe Text
    , _iAttributes :: [Attribute]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Item' data type.
--
-- 'Item' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @AlternateNameEncoding ::@ @Maybe Text@
--
-- * @Attributes ::@ @[Attribute]@
--
item :: Text -- ^ 'iName'
     -> [Attribute] -- ^ 'iAttributes'
     -> Item
item p1 p3 = Item
    { _iName = p1
    , _iAlternateNameEncoding = Nothing
    , _iAttributes = p3
    }

-- | The name of the item.
iName :: Lens' Item Text
iName = lens _iName (\s a -> s { _iName = a })

-- | 
iAlternateNameEncoding :: Lens' Item (Maybe Text)
iAlternateNameEncoding =
    lens _iAlternateNameEncoding (\s a -> s { _iAlternateNameEncoding = a })

-- | A list of attributes.
iAttributes :: Lens' Item [Attribute]
iAttributes = lens _iAttributes (\s a -> s { _iAttributes = a })

instance FromXML Item where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Item"

-- | 
data ReplaceableAttribute = ReplaceableAttribute
    { _raName :: Text
    , _raValue :: Text
    , _raReplace :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReplaceableAttribute' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @Value ::@ @Text@
--
-- * @Replace ::@ @Maybe Bool@
--
replaceableAttribute :: Text -- ^ 'raName'
                     -> Text -- ^ 'raValue'
                     -> ReplaceableAttribute
replaceableAttribute p1 p2 = ReplaceableAttribute
    { _raName = p1
    , _raValue = p2
    , _raReplace = Nothing
    }

-- | The name of the replaceable attribute.
raName :: Lens' ReplaceableAttribute Text
raName = lens _raName (\s a -> s { _raName = a })

-- | The value of the replaceable attribute.
raValue :: Lens' ReplaceableAttribute Text
raValue = lens _raValue (\s a -> s { _raValue = a })

-- | A flag specifying whether or not to replace the attribute/value pair or to
-- add a new attribute/value pair. The default setting is false.
raReplace :: Lens' ReplaceableAttribute (Maybe Bool)
raReplace = lens _raReplace (\s a -> s { _raReplace = a })

instance FromXML ReplaceableAttribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Attribute"

instance ToQuery ReplaceableAttribute where
    toQuery = genericQuery def

-- | 
data ReplaceableItem = ReplaceableItem
    { _riName :: Text
    , _riAttributes :: [ReplaceableAttribute]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReplaceableItem' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @Attributes ::@ @[ReplaceableAttribute]@
--
replaceableItem :: Text -- ^ 'riName'
                -> [ReplaceableAttribute] -- ^ 'riAttributes'
                -> ReplaceableItem
replaceableItem p1 p2 = ReplaceableItem
    { _riName = p1
    , _riAttributes = p2
    }

-- | The name of the replaceable item.
riName :: Lens' ReplaceableItem Text
riName = lens _riName (\s a -> s { _riName = a })

-- | The list of attributes for a replaceable item.
riAttributes :: Lens' ReplaceableItem [ReplaceableAttribute]
riAttributes = lens _riAttributes (\s a -> s { _riAttributes = a })

instance ToQuery ReplaceableItem where
    toQuery = genericQuery def

-- | The update condition which, if specified, determines whether the specified
-- attributes will be updated or not. The update condition must be satisfied
-- in order for this request to be processed and the attributes to be updated.
data UpdateCondition = UpdateCondition
    { _ucName :: Maybe Text
    , _ucValue :: Maybe Text
    , _ucExists :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'UpdateCondition' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
-- * @Value ::@ @Maybe Text@
--
-- * @Exists ::@ @Maybe Bool@
--
updateCondition :: UpdateCondition
updateCondition = UpdateCondition
    { _ucName = Nothing
    , _ucValue = Nothing
    , _ucExists = Nothing
    }

-- | The name of the attribute involved in the condition.
ucName :: Lens' UpdateCondition (Maybe Text)
ucName = lens _ucName (\s a -> s { _ucName = a })

-- | The value of an attribute. This value can only be specified when the Exists
-- parameter is equal to true.
ucValue :: Lens' UpdateCondition (Maybe Text)
ucValue = lens _ucValue (\s a -> s { _ucValue = a })

-- | A value specifying whether or not the specified attribute must exist with
-- the specified value in order for the update condition to be satisfied.
-- Specify true if the attribute must exist for the update condition to be
-- satisfied. Specify false if the attribute should not exist in order for the
-- update condition to be satisfied.
ucExists :: Lens' UpdateCondition (Maybe Bool)
ucExists = lens _ucExists (\s a -> s { _ucExists = a })

instance ToQuery UpdateCondition where
    toQuery = genericQuery def
