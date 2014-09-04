{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetObjectTorrent
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Return torrent files from a bucket.
module Network.AWS.S3.V2006_03_01.GetObjectTorrent
    (
    -- * Request
      GetObjectTorrent
    -- ** Request constructor
    , getObjectTorrent
    -- ** Request lenses
    , gotrBucket
    , gotrKey

    -- * Response
    , GetObjectTorrentResponse
    -- ** Response lenses
    , gotoBody
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetObjectTorrent' request.
getObjectTorrent :: BucketName -- ^ 'gotrBucket'
                 -> ObjectKey -- ^ 'gotrKey'
                 -> GetObjectTorrent
getObjectTorrent p1 p2 = GetObjectTorrent
    { _gotrBucket = p1
    , _gotrKey = p2
    }
{-# INLINE getObjectTorrent #-}

data GetObjectTorrent = GetObjectTorrent
    { _gotrBucket :: BucketName
    , _gotrKey :: ObjectKey
    } deriving (Show, Generic)

gotrBucket :: Lens' GetObjectTorrent (BucketName)
gotrBucket f x =
    f (_gotrBucket x)
        <&> \y -> x { _gotrBucket = y }
{-# INLINE gotrBucket #-}

gotrKey :: Lens' GetObjectTorrent (ObjectKey)
gotrKey f x =
    f (_gotrKey x)
        <&> \y -> x { _gotrKey = y }
{-# INLINE gotrKey #-}

instance ToPath GetObjectTorrent where
    toPath GetObjectTorrent{..} = mconcat
        [ "/"
        , toBS _gotrBucket
        , "/"
        , toBS _gotrKey
        ]

instance ToQuery GetObjectTorrent where
    toQuery GetObjectTorrent{..} = mconcat
        [ "torrent"
        ]

instance ToHeaders GetObjectTorrent

instance ToBody GetObjectTorrent

data GetObjectTorrentResponse = GetObjectTorrentResponse
    { _gotoBody :: RsBody
    } deriving (Show, Generic)

gotoBody :: Lens' GetObjectTorrentResponse (RsBody)
gotoBody f x =
    f (_gotoBody x)
        <&> \y -> x { _gotoBody = y }
{-# INLINE gotoBody #-}

instance AWSRequest GetObjectTorrent where
    type Sv GetObjectTorrent = S3
    type Rs GetObjectTorrent = GetObjectTorrentResponse

    request = get
    response _ = bodyResponse $ \_ bdy ->
        return $! pure GetObjectTorrentResponse
            <*> pure (RsBody bdy)
