{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/ListBuckets.html>
module Network.AWS.S3.ListBuckets
    (
    -- * Request
      ListBuckets
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

import Network.AWS.Prelude
import Network.AWS.Request.RestXML
import Network.AWS.S3.Types
import qualified GHC.Exts

data ListBuckets = ListBuckets
    deriving (Eq, Ord, Show, Generic)

-- | 'ListBuckets' constructor.
listBuckets :: ListBuckets
listBuckets = ListBuckets

data ListBucketsResponse = ListBucketsResponse
    { _lbrBuckets :: [Bucket]
    , _lbrOwner   :: Maybe Owner
    } deriving (Eq, Show, Generic)

-- | 'ListBucketsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lbrBuckets' @::@ ['Bucket']
--
-- * 'lbrOwner' @::@ 'Maybe' 'Owner'
--
listBucketsResponse :: ListBucketsResponse
listBucketsResponse = ListBucketsResponse
    { _lbrBuckets = mempty
    , _lbrOwner   = Nothing
    }

lbrBuckets :: Lens' ListBucketsResponse [Bucket]
lbrBuckets = lens _lbrBuckets (\s a -> s { _lbrBuckets = a })

lbrOwner :: Lens' ListBucketsResponse (Maybe Owner)
lbrOwner = lens _lbrOwner (\s a -> s { _lbrOwner = a })

instance ToPath ListBuckets where
    toPath = const "/"

instance ToQuery ListBuckets where
    toQuery = const mempty

instance ToHeaders ListBuckets

instance ToXMLRoot ListBuckets where
    toXMLRoot = const (element "ListBuckets" [])

instance ToXML ListBuckets

instance AWSRequest ListBuckets where
    type Sv ListBuckets = S3
    type Rs ListBuckets = ListBucketsResponse

    request  = get
    response = xmlResponse

instance FromXML ListBucketsResponse where
    parseXML x = ListBucketsResponse
        <$> x .@ "Buckets"
        <*> x .@? "Owner"
