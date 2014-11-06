{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , gotrBucket
    , gotrKey

    -- * Response
    , GetObjectTorrentOutput
    -- ** Response constructor
    , getObjectTorrentOutput
    -- ** Response lenses
    , gotoBody
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types

data GetObjectTorrent = GetObjectTorrent
    { _gotrBucket :: BucketName
    , _gotrKey    :: ObjectKey
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetObjectTorrent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gotrBucket' @::@ 'BucketName'
--
-- * 'gotrKey' @::@ 'ObjectKey'
--
getObjectTorrent :: BucketName -- ^ 'gotrBucket'
                 -> ObjectKey -- ^ 'gotrKey'
                 -> GetObjectTorrent
getObjectTorrent p1 p2 = GetObjectTorrent
    { _gotrBucket = p1
    , _gotrKey    = p2
    }

gotrBucket :: Lens' GetObjectTorrent BucketName
gotrBucket = lens _gotrBucket (\s a -> s { _gotrBucket = a })

gotrKey :: Lens' GetObjectTorrent ObjectKey
gotrKey = lens _gotrKey (\s a -> s { _gotrKey = a })

instance ToPath GetObjectTorrent where
    toPath GetObjectTorrent{..} = mconcat
        [ "/"
        , toText _gotrBucket
        , "/"
        , toText _gotrKey
        ]

instance ToQuery GetObjectTorrent where
    toQuery = const "torrent"

instance ToHeaders GetObjectTorrent

newtype GetObjectTorrentOutput = GetObjectTorrentOutput
    { _gotoBody :: RsBody
    } deriving (Eq, Show, Generic)

instance AWSRequest GetObjectTorrent where
    type Sv GetObjectTorrent = S3
    type Rs GetObjectTorrent = GetObjectTorrentOutput

    request  = get
    response = const . bodyResponse $ \h b ->
        <$> pure (RsBody b)
