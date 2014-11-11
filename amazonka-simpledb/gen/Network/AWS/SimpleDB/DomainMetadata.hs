{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SimpleDB.DomainMetadata
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
module Network.AWS.SimpleDB.DomainMetadata
    (
    -- * Request
      DomainMetadata
    -- ** Request constructor
    , domainMetadata
    -- ** Request lenses
    , dmDomainName

    -- * Response
    , DomainMetadataResult
    -- ** Response constructor
    , domainMetadataResult
    -- ** Response lenses
    , dmrAttributeNameCount
    , dmrAttributeNamesSizeBytes
    , dmrAttributeValueCount
    , dmrAttributeValuesSizeBytes
    , dmrItemCount
    , dmrItemNamesSizeBytes
    , dmrTimestamp
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SimpleDB.Types

newtype DomainMetadata = DomainMetadata
    { _dmDomainName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DomainMetadata' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmDomainName' @::@ 'Text'
--
domainMetadata :: Text -- ^ 'dmDomainName'
               -> DomainMetadata
domainMetadata p1 = DomainMetadata
    { _dmDomainName = p1
    }

-- | The name of the domain for which to display the metadata of.
dmDomainName :: Lens' DomainMetadata Text
dmDomainName = lens _dmDomainName (\s a -> s { _dmDomainName = a })
instance ToQuery DomainMetadata

instance ToPath DomainMetadata where
    toPath = const "/"

data DomainMetadataResult = DomainMetadataResult
    { _dmrAttributeNameCount       :: Maybe Int
    , _dmrAttributeNamesSizeBytes  :: Maybe Integer
    , _dmrAttributeValueCount      :: Maybe Int
    , _dmrAttributeValuesSizeBytes :: Maybe Integer
    , _dmrItemCount                :: Maybe Int
    , _dmrItemNamesSizeBytes       :: Maybe Integer
    , _dmrTimestamp                :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DomainMetadataResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmrAttributeNameCount' @::@ 'Maybe' 'Int'
--
-- * 'dmrAttributeNamesSizeBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dmrAttributeValueCount' @::@ 'Maybe' 'Int'
--
-- * 'dmrAttributeValuesSizeBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dmrItemCount' @::@ 'Maybe' 'Int'
--
-- * 'dmrItemNamesSizeBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dmrTimestamp' @::@ 'Maybe' 'Int'
--
domainMetadataResult :: DomainMetadataResult
domainMetadataResult = DomainMetadataResult
    { _dmrItemCount                = Nothing
    , _dmrItemNamesSizeBytes       = Nothing
    , _dmrAttributeNameCount       = Nothing
    , _dmrAttributeNamesSizeBytes  = Nothing
    , _dmrAttributeValueCount      = Nothing
    , _dmrAttributeValuesSizeBytes = Nothing
    , _dmrTimestamp                = Nothing
    }

-- | The number of unique attribute names in the domain.
dmrAttributeNameCount :: Lens' DomainMetadataResult (Maybe Int)
dmrAttributeNameCount =
    lens _dmrAttributeNameCount (\s a -> s { _dmrAttributeNameCount = a })

-- | The total size of all unique attribute names in the domain, in bytes.
dmrAttributeNamesSizeBytes :: Lens' DomainMetadataResult (Maybe Integer)
dmrAttributeNamesSizeBytes =
    lens _dmrAttributeNamesSizeBytes
        (\s a -> s { _dmrAttributeNamesSizeBytes = a })

-- | The number of all attribute name/value pairs in the domain.
dmrAttributeValueCount :: Lens' DomainMetadataResult (Maybe Int)
dmrAttributeValueCount =
    lens _dmrAttributeValueCount (\s a -> s { _dmrAttributeValueCount = a })

-- | The total size of all attribute values in the domain, in bytes.
dmrAttributeValuesSizeBytes :: Lens' DomainMetadataResult (Maybe Integer)
dmrAttributeValuesSizeBytes =
    lens _dmrAttributeValuesSizeBytes
        (\s a -> s { _dmrAttributeValuesSizeBytes = a })

-- | The number of all items in the domain.
dmrItemCount :: Lens' DomainMetadataResult (Maybe Int)
dmrItemCount = lens _dmrItemCount (\s a -> s { _dmrItemCount = a })

-- | The total size of all item names in the domain, in bytes.
dmrItemNamesSizeBytes :: Lens' DomainMetadataResult (Maybe Integer)
dmrItemNamesSizeBytes =
    lens _dmrItemNamesSizeBytes (\s a -> s { _dmrItemNamesSizeBytes = a })

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
dmrTimestamp :: Lens' DomainMetadataResult (Maybe Int)
dmrTimestamp = lens _dmrTimestamp (\s a -> s { _dmrTimestamp = a })
instance FromXML DomainMetadataResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DomainMetadataResult"

instance AWSRequest DomainMetadata where
    type Sv DomainMetadata = SimpleDB
    type Rs DomainMetadata = DomainMetadataResult

    request  = post "DomainMetadata"
    response = xmlResponse $ \h x -> DomainMetadataResult
        <$> x %| "AttributeNameCount"
        <*> x %| "AttributeNamesSizeBytes"
        <*> x %| "AttributeValueCount"
        <*> x %| "AttributeValuesSizeBytes"
        <*> x %| "ItemCount"
        <*> x %| "ItemNamesSizeBytes"
        <*> x %| "Timestamp"
