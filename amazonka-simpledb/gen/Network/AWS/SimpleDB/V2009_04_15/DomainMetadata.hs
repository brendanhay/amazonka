{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.DomainMetadata
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the domain, including when the domain was
-- created, the number of items and attributes in the domain, and the size of
-- the attribute names and values.
module Network.AWS.SimpleDB.V2009_04_15.DomainMetadata
    (
    -- * Request
      DomainMetadata
    -- ** Request constructor
    , domainMetadata
    -- ** Request lenses
    , dmrDomainName

    -- * Response
    , DomainMetadataResponse
    -- ** Response lenses
    , dmsItemCount
    , dmsAttributeNameCount
    , dmsAttributeValueCount
    , dmsTimestamp
    , dmsItemNamesSizeBytes
    , dmsAttributeNamesSizeBytes
    , dmsAttributeValuesSizeBytes
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DomainMetadata' request.
domainMetadata :: Text -- ^ 'dmrDomainName'
               -> DomainMetadata
domainMetadata p1 = DomainMetadata
    { _dmrDomainName = p1
    }

data DomainMetadata = DomainMetadata
    { _dmrDomainName :: Text
      -- ^ The name of the domain for which to display the metadata of.
    } deriving (Show, Generic)

-- | The name of the domain for which to display the metadata of.
dmrDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> DomainMetadata
    -> f DomainMetadata
dmrDomainName f x =
    (\y -> x { _dmrDomainName = y })
       <$> f (_dmrDomainName x)
{-# INLINE dmrDomainName #-}

instance ToQuery DomainMetadata where
    toQuery = genericQuery def

data DomainMetadataResponse = DomainMetadataResponse
    { _dmsItemCount :: Maybe Integer
      -- ^ The number of all items in the domain.
    , _dmsAttributeNameCount :: Maybe Integer
      -- ^ The number of unique attribute names in the domain.
    , _dmsAttributeValueCount :: Maybe Integer
      -- ^ The number of all attribute name/value pairs in the domain.
    , _dmsTimestamp :: Maybe Integer
      -- ^ The data and time when metadata was calculated, in Epoch (UNIX)
      -- seconds.
    , _dmsItemNamesSizeBytes :: Maybe Integer
      -- ^ The total size of all item names in the domain, in bytes.
    , _dmsAttributeNamesSizeBytes :: Maybe Integer
      -- ^ The total size of all unique attribute names in the domain, in
      -- bytes.
    , _dmsAttributeValuesSizeBytes :: Maybe Integer
      -- ^ The total size of all attribute values in the domain, in bytes.
    } deriving (Show, Generic)

-- | The number of all items in the domain.
dmsItemCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DomainMetadataResponse
    -> f DomainMetadataResponse
dmsItemCount f x =
    (\y -> x { _dmsItemCount = y })
       <$> f (_dmsItemCount x)
{-# INLINE dmsItemCount #-}

-- | The number of unique attribute names in the domain.
dmsAttributeNameCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DomainMetadataResponse
    -> f DomainMetadataResponse
dmsAttributeNameCount f x =
    (\y -> x { _dmsAttributeNameCount = y })
       <$> f (_dmsAttributeNameCount x)
{-# INLINE dmsAttributeNameCount #-}

-- | The number of all attribute name/value pairs in the domain.
dmsAttributeValueCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DomainMetadataResponse
    -> f DomainMetadataResponse
dmsAttributeValueCount f x =
    (\y -> x { _dmsAttributeValueCount = y })
       <$> f (_dmsAttributeValueCount x)
{-# INLINE dmsAttributeValueCount #-}

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
dmsTimestamp
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DomainMetadataResponse
    -> f DomainMetadataResponse
dmsTimestamp f x =
    (\y -> x { _dmsTimestamp = y })
       <$> f (_dmsTimestamp x)
{-# INLINE dmsTimestamp #-}

-- | The total size of all item names in the domain, in bytes.
dmsItemNamesSizeBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DomainMetadataResponse
    -> f DomainMetadataResponse
dmsItemNamesSizeBytes f x =
    (\y -> x { _dmsItemNamesSizeBytes = y })
       <$> f (_dmsItemNamesSizeBytes x)
{-# INLINE dmsItemNamesSizeBytes #-}

-- | The total size of all unique attribute names in the domain, in bytes.
dmsAttributeNamesSizeBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DomainMetadataResponse
    -> f DomainMetadataResponse
dmsAttributeNamesSizeBytes f x =
    (\y -> x { _dmsAttributeNamesSizeBytes = y })
       <$> f (_dmsAttributeNamesSizeBytes x)
{-# INLINE dmsAttributeNamesSizeBytes #-}

-- | The total size of all attribute values in the domain, in bytes.
dmsAttributeValuesSizeBytes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> DomainMetadataResponse
    -> f DomainMetadataResponse
dmsAttributeValuesSizeBytes f x =
    (\y -> x { _dmsAttributeValuesSizeBytes = y })
       <$> f (_dmsAttributeValuesSizeBytes x)
{-# INLINE dmsAttributeValuesSizeBytes #-}

instance FromXML DomainMetadataResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DomainMetadata where
    type Sv DomainMetadata = SimpleDB
    type Rs DomainMetadata = DomainMetadataResponse

    request = post "DomainMetadata"
    response _ = xmlResponse
