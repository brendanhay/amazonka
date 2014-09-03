{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.GetAttributes
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
-- item does not exist on other replicas. If GetAttributes is called without
-- being passed any attribute names, all the attributes for the item are
-- returned.
module Network.AWS.SimpleDB.V2009_04_15.GetAttributes
    (
    -- * Request
      GetAttributes
    -- ** Request constructor
    , getAttributes
    -- ** Request lenses
    , garDomainName
    , garItemName
    , garAttributeNames
    , garConsistentRead

    -- * Response
    , GetAttributesResponse
    -- ** Response lenses
    , gasAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetAttributes' request.
getAttributes :: Text -- ^ 'garDomainName'
              -> Text -- ^ 'garItemName'
              -> GetAttributes
getAttributes p1 p2 = GetAttributes
    { _garDomainName = p1
    , _garItemName = p2
    , _garAttributeNames = mempty
    , _garConsistentRead = Nothing
    }

data GetAttributes = GetAttributes
    { _garDomainName :: Text
      -- ^ The name of the domain in which to perform the operation.
    , _garItemName :: Text
      -- ^ The name of the item.
    , _garAttributeNames :: [Text]
      -- ^ The names of the attributes.
    , _garConsistentRead :: Maybe Bool
      -- ^ Determines whether or not strong consistency should be enforced
      -- when data is read from SimpleDB. If true, any data previously
      -- written to SimpleDB will be returned. Otherwise, results will be
      -- consistent eventually, and the client may not see data that was
      -- written immediately before your read.
    } deriving (Show, Generic)

-- | The name of the domain in which to perform the operation.
garDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> GetAttributes
    -> f GetAttributes
garDomainName f x =
    (\y -> x { _garDomainName = y })
       <$> f (_garDomainName x)
{-# INLINE garDomainName #-}

-- | The name of the item.
garItemName
    :: Functor f
    => (Text
    -> f (Text))
    -> GetAttributes
    -> f GetAttributes
garItemName f x =
    (\y -> x { _garItemName = y })
       <$> f (_garItemName x)
{-# INLINE garItemName #-}

-- | The names of the attributes.
garAttributeNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> GetAttributes
    -> f GetAttributes
garAttributeNames f x =
    (\y -> x { _garAttributeNames = y })
       <$> f (_garAttributeNames x)
{-# INLINE garAttributeNames #-}

-- | Determines whether or not strong consistency should be enforced when data
-- is read from SimpleDB. If true, any data previously written to SimpleDB
-- will be returned. Otherwise, results will be consistent eventually, and the
-- client may not see data that was written immediately before your read.
garConsistentRead
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> GetAttributes
    -> f GetAttributes
garConsistentRead f x =
    (\y -> x { _garConsistentRead = y })
       <$> f (_garConsistentRead x)
{-# INLINE garConsistentRead #-}

instance ToQuery GetAttributes where
    toQuery = genericQuery def

data GetAttributesResponse = GetAttributesResponse
    { _gasAttributes :: [Attribute]
      -- ^ The list of attributes returned by the operation.
    } deriving (Show, Generic)

-- | The list of attributes returned by the operation.
gasAttributes
    :: Functor f
    => ([Attribute]
    -> f ([Attribute]))
    -> GetAttributesResponse
    -> f GetAttributesResponse
gasAttributes f x =
    (\y -> x { _gasAttributes = y })
       <$> f (_gasAttributes x)
{-# INLINE gasAttributes #-}

instance FromXML GetAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetAttributes where
    type Sv GetAttributes = SimpleDB
    type Rs GetAttributes = GetAttributesResponse

    request = post "GetAttributes"
    response _ = xmlResponse
