{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
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
    ( module Network.AWS.SimpleDB.V2009_04_15.Types
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
      -- ^ The name of the attribute.
    , _aAlternateNameEncoding :: Maybe Text
      -- ^ 
    , _aValue :: Text
      -- ^ The value of the attribute.
    , _aAlternateValueEncoding :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

instance FromXML Attribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Attribute"

instance ToQuery Attribute where
    toQuery = genericQuery def

data DeletableItem = DeletableItem
    { _diName :: Text
    , _diAttributes :: [Attribute]
    } deriving (Show, Generic)

instance ToQuery DeletableItem where
    toQuery = genericQuery def

-- | 
data Item = Item
    { _pName :: Text
      -- ^ The name of the item.
    , _pAttributes :: [Attribute]
      -- ^ A list of attributes.
    , _pAlternateNameEncoding :: Maybe Text
      -- ^ 
    } deriving (Show, Generic)

instance FromXML Item where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Item"

-- | 
data ReplaceableAttribute = ReplaceableAttribute
    { _raName :: Text
      -- ^ The name of the replaceable attribute.
    , _raValue :: Text
      -- ^ The value of the replaceable attribute.
    , _raReplace :: Maybe Bool
      -- ^ A flag specifying whether or not to replace the attribute/value
      -- pair or to add a new attribute/value pair. The default setting is
      -- false.
    } deriving (Show, Generic)

instance FromXML ReplaceableAttribute where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Attribute"

instance ToQuery ReplaceableAttribute where
    toQuery = genericQuery def

-- | 
data ReplaceableItem = ReplaceableItem
    { _riName :: Text
      -- ^ The name of the replaceable item.
    , _riAttributes :: [ReplaceableAttribute]
      -- ^ The list of attributes for a replaceable item.
    } deriving (Show, Generic)

instance ToQuery ReplaceableItem where
    toQuery = genericQuery def

-- | The update condition which, if specified, determines whether the specified
-- attributes will be deleted or not. The update condition must be satisfied
-- in order for this request to be processed and the attributes to be deleted.
data UpdateCondition = UpdateCondition
    { _ucName :: Maybe Text
      -- ^ The name of the attribute involved in the condition.
    , _ucValue :: Maybe Text
      -- ^ The value of an attribute. This value can only be specified when
      -- the Exists parameter is equal to true.
    , _ucExists :: Maybe Bool
      -- ^ A value specifying whether or not the specified attribute must
      -- exist with the specified value in order for the update condition
      -- to be satisfied. Specify true if the attribute must exist for the
      -- update condition to be satisfied. Specify false if the attribute
      -- should not exist in order for the update condition to be
      -- satisfied.
    } deriving (Show, Generic)

instance ToQuery UpdateCondition where
    toQuery = genericQuery def

makeLenses ''Attribute
makeLenses ''DeletableItem
makeLenses ''Item
makeLenses ''ReplaceableAttribute
makeLenses ''ReplaceableItem
makeLenses ''UpdateCondition
