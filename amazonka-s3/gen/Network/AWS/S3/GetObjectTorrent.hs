{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetObjectTorrent.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types
import qualified GHC.Exts

data GetObjectTorrent = GetObjectTorrent
    { _gotBucket :: Text
    , _gotKey    :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetObjectTorrent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gotBucket' @::@ 'Text'
--
-- * 'gotKey' @::@ 'Text'
--
getObjectTorrent :: Text -- ^ 'gotBucket'
                 -> Text -- ^ 'gotKey'
                 -> GetObjectTorrent
getObjectTorrent p1 p2 = GetObjectTorrent
    { _gotBucket = p1
    , _gotKey    = p2
    }

gotBucket :: Lens' GetObjectTorrent Text
gotBucket = lens _gotBucket (\s a -> s { _gotBucket = a })

gotKey :: Lens' GetObjectTorrent Text
gotKey = lens _gotKey (\s a -> s { _gotKey = a })

newtype GetObjectTorrentResponse = GetObjectTorrentResponse
    { _gotrBody :: Maybe Base64
    } deriving (Eq, Show, Generic)

-- | 'GetObjectTorrentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gotrBody' @::@ 'Maybe' 'Base64'
--
getObjectTorrentResponse :: GetObjectTorrentResponse
getObjectTorrentResponse = GetObjectTorrentResponse
    { _gotrBody = Nothing
    }

gotrBody :: Lens' GetObjectTorrentResponse (Maybe Base64)
gotrBody = lens _gotrBody (\s a -> s { _gotrBody = a })

instance ToPath GetObjectTorrent where
    toPath GetObjectTorrent{..} = mconcat
        [ "/"
        , toText _gotBucket
        , "/"
        , toText _gotKey
        ]

instance ToQuery GetObjectTorrent where
    toQuery = const "torrent"

instance ToHeaders GetObjectTorrent
instance ToXML GetObjectTorrent where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetObjectTorrent"

instance AWSRequest GetObjectTorrent where
    type Sv GetObjectTorrent = S3
    type Rs GetObjectTorrent = GetObjectTorrentResponse

    request  = get
    response = bodyResponse . const $ \b -> GetObjectTorrentResponse
