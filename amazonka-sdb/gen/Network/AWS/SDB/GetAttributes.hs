{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SDB.GetAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all of the attributes associated with the specified item.
-- Optionally, the attributes returned can be limited to one or more
-- attributes by specifying an attribute name parameter. If the item does not
-- exist on the replica that was accessed for this operation, an empty set is
-- returned. The system does not return an error as it cannot guarantee the
-- item does not exist on other replicas.
module Network.AWS.SDB.GetAttributes
    (
    -- * Request
      GetAttributes
    -- ** Request constructor
    , getAttributes
    -- ** Request lenses
    , gaAttributeNames
    , gaConsistentRead
    , gaDomainName
    , gaItemName

    -- * Response
    , GetAttributesResponse
    -- ** Response constructor
    , getAttributesResponse
    -- ** Response lenses
    , garAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SDB.Types
import qualified GHC.Exts

data GetAttributes = GetAttributes
    { _gaAttributeNames :: [Text]
    , _gaConsistentRead :: Maybe Bool
    , _gaDomainName     :: Text
    , _gaItemName       :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gaAttributeNames' @::@ ['Text']
--
-- * 'gaConsistentRead' @::@ 'Maybe' 'Bool'
--
-- * 'gaDomainName' @::@ 'Text'
--
-- * 'gaItemName' @::@ 'Text'
--
getAttributes :: Text -- ^ 'gaDomainName'
              -> Text -- ^ 'gaItemName'
              -> GetAttributes
getAttributes p1 p2 = GetAttributes
    { _gaDomainName     = p1
    , _gaItemName       = p2
    , _gaAttributeNames = mempty
    , _gaConsistentRead = Nothing
    }

-- | The names of the attributes.
gaAttributeNames :: Lens' GetAttributes [Text]
gaAttributeNames = lens _gaAttributeNames (\s a -> s { _gaAttributeNames = a })

-- | Determines whether or not strong consistency should be enforced when data
-- is read from SimpleDB. If true, any data previously written to SimpleDB
-- will be returned. Otherwise, results will be consistent eventually, and
-- the client may not see data that was written immediately before your
-- read.
gaConsistentRead :: Lens' GetAttributes (Maybe Bool)
gaConsistentRead = lens _gaConsistentRead (\s a -> s { _gaConsistentRead = a })

-- | The name of the domain in which to perform the operation.
gaDomainName :: Lens' GetAttributes Text
gaDomainName = lens _gaDomainName (\s a -> s { _gaDomainName = a })

-- | The name of the item.
gaItemName :: Lens' GetAttributes Text
gaItemName = lens _gaItemName (\s a -> s { _gaItemName = a })

newtype GetAttributesResponse = GetAttributesResponse
    { _garAttributes :: [Attribute]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList GetAttributesResponse where
    type Item GetAttributesResponse = Attribute

    fromList = GetAttributesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _garAttributes

-- | 'GetAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'garAttributes' @::@ ['Attribute']
--
getAttributesResponse :: GetAttributesResponse
getAttributesResponse = GetAttributesResponse
    { _garAttributes = mempty
    }

-- | The list of attributes returned by the operation.
garAttributes :: Lens' GetAttributesResponse [Attribute]
garAttributes = lens _garAttributes (\s a -> s { _garAttributes = a })

instance AWSRequest GetAttributes where
    type Sv GetAttributes = SDB
    type Rs GetAttributes = GetAttributesResponse

    request  = post "GetAttributes"
    response = xmlResponse

instance FromXML GetAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetAttributesResponse"

instance ToPath GetAttributes where
    toPath = const "/"

instance ToHeaders GetAttributes

instance ToQuery GetAttributes
