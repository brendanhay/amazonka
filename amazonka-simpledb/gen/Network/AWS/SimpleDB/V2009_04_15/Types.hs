{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.Types
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
module Network.AWS.SimpleDB.V2009_04_15.Types
    (
    -- * Service
      SimpleDB
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * Attribute
    , Attribute
    , mkAttribute
    , aName
    , aAlternateNameEncoding
    , aValue
    , aAlternateValueEncoding

    -- * DeletableItem
    , DeletableItem
    , mkDeletableItem
    , diName
    , diAttributes

    -- * Item
    , Item
    , mkItem
    , iName
    , iAlternateNameEncoding
    , iAttributes

    -- * ReplaceableAttribute
    , ReplaceableAttribute
    , mkReplaceableAttribute
    , raName
    , raValue
    , raReplace

    -- * ReplaceableItem
    , ReplaceableItem
    , mkReplaceableItem
    , riName
    , riAttributes

    -- * UpdateCondition
    , UpdateCondition
    , mkUpdateCondition
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
    data Er SimpleDB
        = AttributeDoesNotExist
            { _adneBoxUsage :: Maybe Double
            }
        | DuplicateItemName
            { _dinBoxUsage :: Maybe Double
            }
        | InvalidNextToken
            { _intBoxUsage :: Maybe Double
            }
        | InvalidNumberPredicates
            { _inpBoxUsage :: Maybe Double
            }
        | InvalidNumberValueTests
            { _invtBoxUsage :: Maybe Double
            }
        | InvalidParameterValue
            { _ipvBoxUsage :: Maybe Double
            }
        | InvalidQueryExpression
            { _iqeBoxUsage :: Maybe Double
            }
        | MissingParameter
            { _mpBoxUsage :: Maybe Double
            }
        | NoSuchDomain
            { _nsdBoxUsage :: Maybe Double
            }
        | NumberDomainAttributesExceeded
            { _ndaeBoxUsage :: Maybe Double
            }
        | NumberDomainBytesExceeded
            { _ndbeBoxUsage :: Maybe Double
            }
        | NumberDomainsExceeded
            { _ndeBoxUsage :: Maybe Double
            }
        | NumberItemAttributesExceeded
            { _niaeBoxUsage :: Maybe Double
            }
        | NumberSubmittedAttributesExceeded
            { _nsaeBoxUsage :: Maybe Double
            }
        | NumberSubmittedItemsExceeded
            { _nsieBoxUsage :: Maybe Double
            }
        | RequestTimeout
            { _rtBoxUsage :: Maybe Double
            }
        | SimpleDBClient HttpException
        | SimpleDBSerializer String
        | SimpleDBService String
        | TooManyRequestedAttributes
            { _tmraBoxUsage :: Maybe Double
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "sdb"
        , _svcVersion  = "2009-04-15"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er SimpleDB)
deriving instance Generic (Er SimpleDB)

instance AWSError (Er SimpleDB) where
    awsError = const "SimpleDBError"

instance AWSServiceError (Er SimpleDB) where
    serviceError    = SimpleDBService
    clientError     = SimpleDBClient
    serializerError = SimpleDBSerializer

instance Exception (Er SimpleDB)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://sdb.amazonaws.com/doc/2009-04-15/"
    }

-- | 
data Attribute = Attribute
    { _aName :: Text
    , _aAlternateNameEncoding :: Maybe Text
    , _aValue :: Text
    , _aAlternateValueEncoding :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Attribute' data type to populate a request.
mkAttribute :: Text -- ^ 'aName'
            -> Text -- ^ 'aValue'
            -> Attribute
mkAttribute p1 p3 = Attribute
    { _aName = p1
    , _aAlternateNameEncoding = Nothing
    , _aValue = p3
    , _aAlternateValueEncoding = Nothing
    }
{-# INLINE mkAttribute #-}

-- | The name of the attribute.
aName :: Lens' Attribute Text
aName = lens _aName (\s a -> s { _aName = a })
{-# INLINE aName #-}

-- | 
aAlternateNameEncoding :: Lens' Attribute (Maybe Text)
aAlternateNameEncoding =
    lens _aAlternateNameEncoding (\s a -> s { _aAlternateNameEncoding = a })
{-# INLINE aAlternateNameEncoding #-}

-- | The value of the attribute.
aValue :: Lens' Attribute Text
aValue = lens _aValue (\s a -> s { _aValue = a })
{-# INLINE aValue #-}

-- | 
aAlternateValueEncoding :: Lens' Attribute (Maybe Text)
aAlternateValueEncoding =
    lens _aAlternateValueEncoding
         (\s a -> s { _aAlternateValueEncoding = a })
{-# INLINE aAlternateValueEncoding #-}

instance FromXML Attribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Attribute"

instance ToQuery Attribute where
    toQuery = genericQuery def

data DeletableItem = DeletableItem
    { _diName :: Text
    , _diAttributes :: [Attribute]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DeletableItem' data type to populate a request.
mkDeletableItem :: Text -- ^ 'diName'
                -> DeletableItem
mkDeletableItem p1 = DeletableItem
    { _diName = p1
    , _diAttributes = mempty
    }
{-# INLINE mkDeletableItem #-}

diName :: Lens' DeletableItem Text
diName = lens _diName (\s a -> s { _diName = a })
{-# INLINE diName #-}

diAttributes :: Lens' DeletableItem [Attribute]
diAttributes = lens _diAttributes (\s a -> s { _diAttributes = a })
{-# INLINE diAttributes #-}

instance ToQuery DeletableItem where
    toQuery = genericQuery def

-- | 
data Item = Item
    { _iName :: Text
    , _iAlternateNameEncoding :: Maybe Text
    , _iAttributes :: [Attribute]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Item' data type to populate a request.
mkItem :: Text -- ^ 'iName'
       -> [Attribute] -- ^ 'iAttributes'
       -> Item
mkItem p1 p3 = Item
    { _iName = p1
    , _iAlternateNameEncoding = Nothing
    , _iAttributes = p3
    }
{-# INLINE mkItem #-}

-- | The name of the item.
iName :: Lens' Item Text
iName = lens _iName (\s a -> s { _iName = a })
{-# INLINE iName #-}

-- | 
iAlternateNameEncoding :: Lens' Item (Maybe Text)
iAlternateNameEncoding =
    lens _iAlternateNameEncoding (\s a -> s { _iAlternateNameEncoding = a })
{-# INLINE iAlternateNameEncoding #-}

-- | A list of attributes.
iAttributes :: Lens' Item [Attribute]
iAttributes = lens _iAttributes (\s a -> s { _iAttributes = a })
{-# INLINE iAttributes #-}

instance FromXML Item where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Item"

-- | 
data ReplaceableAttribute = ReplaceableAttribute
    { _raName :: Text
    , _raValue :: Text
    , _raReplace :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReplaceableAttribute' data type to populate a request.
mkReplaceableAttribute :: Text -- ^ 'raName'
                       -> Text -- ^ 'raValue'
                       -> ReplaceableAttribute
mkReplaceableAttribute p1 p2 = ReplaceableAttribute
    { _raName = p1
    , _raValue = p2
    , _raReplace = Nothing
    }
{-# INLINE mkReplaceableAttribute #-}

-- | The name of the replaceable attribute.
raName :: Lens' ReplaceableAttribute Text
raName = lens _raName (\s a -> s { _raName = a })
{-# INLINE raName #-}

-- | The value of the replaceable attribute.
raValue :: Lens' ReplaceableAttribute Text
raValue = lens _raValue (\s a -> s { _raValue = a })
{-# INLINE raValue #-}

-- | A flag specifying whether or not to replace the attribute/value pair or to
-- add a new attribute/value pair. The default setting is false.
raReplace :: Lens' ReplaceableAttribute (Maybe Bool)
raReplace = lens _raReplace (\s a -> s { _raReplace = a })
{-# INLINE raReplace #-}

instance FromXML ReplaceableAttribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Attribute"

instance ToQuery ReplaceableAttribute where
    toQuery = genericQuery def

-- | 
data ReplaceableItem = ReplaceableItem
    { _riName :: Text
    , _riAttributes :: [ReplaceableAttribute]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReplaceableItem' data type to populate a request.
mkReplaceableItem :: Text -- ^ 'riName'
                  -> [ReplaceableAttribute] -- ^ 'riAttributes'
                  -> ReplaceableItem
mkReplaceableItem p1 p2 = ReplaceableItem
    { _riName = p1
    , _riAttributes = p2
    }
{-# INLINE mkReplaceableItem #-}

-- | The name of the replaceable item.
riName :: Lens' ReplaceableItem Text
riName = lens _riName (\s a -> s { _riName = a })
{-# INLINE riName #-}

-- | The list of attributes for a replaceable item.
riAttributes :: Lens' ReplaceableItem [ReplaceableAttribute]
riAttributes = lens _riAttributes (\s a -> s { _riAttributes = a })
{-# INLINE riAttributes #-}

instance ToQuery ReplaceableItem where
    toQuery = genericQuery def

-- | The update condition which, if specified, determines whether the specified
-- attributes will be deleted or not. The update condition must be satisfied
-- in order for this request to be processed and the attributes to be deleted.
data UpdateCondition = UpdateCondition
    { _ucName :: Maybe Text
    , _ucValue :: Maybe Text
    , _ucExists :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'UpdateCondition' data type to populate a request.
mkUpdateCondition :: UpdateCondition
mkUpdateCondition = UpdateCondition
    { _ucName = Nothing
    , _ucValue = Nothing
    , _ucExists = Nothing
    }
{-# INLINE mkUpdateCondition #-}

-- | The name of the attribute involved in the condition.
ucName :: Lens' UpdateCondition (Maybe Text)
ucName = lens _ucName (\s a -> s { _ucName = a })
{-# INLINE ucName #-}

-- | The value of an attribute. This value can only be specified when the Exists
-- parameter is equal to true.
ucValue :: Lens' UpdateCondition (Maybe Text)
ucValue = lens _ucValue (\s a -> s { _ucValue = a })
{-# INLINE ucValue #-}

-- | A value specifying whether or not the specified attribute must exist with
-- the specified value in order for the update condition to be satisfied.
-- Specify true if the attribute must exist for the update condition to be
-- satisfied. Specify false if the attribute should not exist in order for the
-- update condition to be satisfied.
ucExists :: Lens' UpdateCondition (Maybe Bool)
ucExists = lens _ucExists (\s a -> s { _ucExists = a })
{-# INLINE ucExists #-}

instance ToQuery UpdateCondition where
    toQuery = genericQuery def
