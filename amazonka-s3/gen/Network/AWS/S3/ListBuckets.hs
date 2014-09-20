{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.ListBuckets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all buckets owned by the authenticated sender of the
-- request.
module Network.AWS.S3.ListBuckets
    (
    -- * Request
      ListBuckets
    -- ** Request alias
    , GetService
    -- ** Request constructor
    , listBuckets
    -- * Response
    , ListBucketsResponse
    -- ** Response constructor
    , listBucketsResponse
    -- ** Response lenses
    , lbrBuckets
    , lbrOwner
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

type GetService = ListBuckets

data ListBuckets = ListBuckets
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListBuckets' request.
listBuckets :: ListBuckets
listBuckets = ListBuckets

instance ToPath ListBuckets

instance ToQuery ListBuckets

instance ToHeaders ListBuckets

instance ToBody ListBuckets

data ListBucketsResponse = ListBucketsResponse
    { _lbrBuckets :: [Bucket]
    , _lbrOwner :: Maybe Owner
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListBucketsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Buckets ::@ @[Bucket]@
--
-- * @Owner ::@ @Maybe Owner@
--
listBucketsResponse :: ListBucketsResponse
listBucketsResponse = ListBucketsResponse
    { _lbrBuckets = mempty
    , _lbrOwner = Nothing
    }

lbrBuckets :: Lens' ListBucketsResponse [Bucket]
lbrBuckets = lens _lbrBuckets (\s a -> s { _lbrBuckets = a })

lbrOwner :: Lens' ListBucketsResponse (Maybe Owner)
lbrOwner = lens _lbrOwner (\s a -> s { _lbrOwner = a })

instance FromXML ListBucketsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListBuckets where
    type Sv ListBuckets = S3
    type Rs ListBuckets = ListBucketsResponse

    request = get
    response _ = xmlResponse
