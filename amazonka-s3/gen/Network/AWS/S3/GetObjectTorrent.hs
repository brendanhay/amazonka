{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetObjectTorrent
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Return torrent files from a bucket.
module Network.AWS.S3.GetObjectTorrent
    (
    -- * Request
      GetObjectTorrent
    -- ** Request constructor
    , getObjectTorrent
    -- ** Request lenses
    , gotBucket
    , gotKey

    -- * Response
    , GetObjectTorrentResponse
    -- ** Response constructor
    , getObjectTorrentResponse
    -- ** Response lenses
    , gotrBody
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

data GetObjectTorrent = GetObjectTorrent
    { _gotBucket :: BucketName
    , _gotKey :: ObjectKey
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObjectTorrent' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @BucketName@
--
-- * @Key ::@ @ObjectKey@
--
getObjectTorrent :: BucketName -- ^ 'gotBucket'
                 -> ObjectKey -- ^ 'gotKey'
                 -> GetObjectTorrent
getObjectTorrent p1 p2 = GetObjectTorrent
    { _gotBucket = p1
    , _gotKey = p2
    }

gotBucket :: Lens' GetObjectTorrent BucketName
gotBucket = lens _gotBucket (\s a -> s { _gotBucket = a })

gotKey :: Lens' GetObjectTorrent ObjectKey
gotKey = lens _gotKey (\s a -> s { _gotKey = a })

instance ToPath GetObjectTorrent

instance ToQuery GetObjectTorrent

instance ToHeaders GetObjectTorrent

instance ToBody GetObjectTorrent

newtype GetObjectTorrentResponse = GetObjectTorrentResponse
    { _gotrBody :: RsBody
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetObjectTorrentResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Body ::@ @RsBody@
--
getObjectTorrentResponse :: RsBody -- ^ 'gotrBody'
                         -> GetObjectTorrentResponse
getObjectTorrentResponse p1 = GetObjectTorrentResponse
    { _gotrBody = p1
    }

gotrBody :: Lens' GetObjectTorrentResponse RsBody
gotrBody = lens _gotrBody (\s a -> s { _gotrBody = a })

instance AWSRequest GetObjectTorrent where
    type Sv GetObjectTorrent = S3
    type Rs GetObjectTorrent = GetObjectTorrentResponse

    request = get
    response _ = bodyResponse $ \_ bdy ->
        return $! pure GetObjectTorrentResponse
            <*> pure (RsBody bdy)
