{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.S3.V2006_03_01.GetObjectTorrent where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

data GetObjectTorrent = GetObjectTorrent
    { _gotrBucket :: BucketName
    , _gotrKey :: ObjectKey
    } deriving (Show, Generic)

makeLenses ''GetObjectTorrent

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

makeLenses ''GetObjectTorrentResponse

instance AWSRequest GetObjectTorrent where
    type Sv GetObjectTorrent = S3
    type Rs GetObjectTorrent = GetObjectTorrentResponse

    request = get
    response _ = bodyResponse $ \_ bdy ->
        return $! pure GetObjectTorrentResponse
            <*> pure (RsBody bdy)
