{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , gotRequestPayer

    -- * Response
    , GetObjectTorrentResponse
    -- ** Response constructor
    , getObjectTorrentResponse
    -- ** Response lenses
    , gotrBody
    , gotrRequestCharged
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data GetObjectTorrent = GetObjectTorrent
    { _gotBucket       :: Text
    , _gotKey          :: Text
    , _gotRequestPayer :: Maybe RequestPayer
    } deriving (Eq, Read, Show)

-- | 'GetObjectTorrent' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gotBucket' @::@ 'Text'
--
-- * 'gotKey' @::@ 'Text'
--
-- * 'gotRequestPayer' @::@ 'Maybe' 'RequestPayer'
--
getObjectTorrent :: Text -- ^ 'gotBucket'
                 -> Text -- ^ 'gotKey'
                 -> GetObjectTorrent
getObjectTorrent p1 p2 = GetObjectTorrent
    { _gotBucket       = p1
    , _gotKey          = p2
    , _gotRequestPayer = Nothing
    }

gotBucket :: Lens' GetObjectTorrent Text
gotBucket = lens _gotBucket (\s a -> s { _gotBucket = a })

gotKey :: Lens' GetObjectTorrent Text
gotKey = lens _gotKey (\s a -> s { _gotKey = a })

gotRequestPayer :: Lens' GetObjectTorrent (Maybe RequestPayer)
gotRequestPayer = lens _gotRequestPayer (\s a -> s { _gotRequestPayer = a })

data GetObjectTorrentResponse = GetObjectTorrentResponse
    { _gotrBody           :: RsBody
    , _gotrRequestCharged :: Maybe RequestCharged
    } deriving (Show)

-- | 'GetObjectTorrentResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gotrBody' @::@ 'RsBody'
--
-- * 'gotrRequestCharged' @::@ 'Maybe' 'RequestCharged'
--
getObjectTorrentResponse :: RsBody -- ^ 'gotrBody'
                         -> GetObjectTorrentResponse
getObjectTorrentResponse p1 = GetObjectTorrentResponse
    { _gotrBody           = p1
    , _gotrRequestCharged = Nothing
    }

gotrBody :: Lens' GetObjectTorrentResponse RsBody
gotrBody = lens _gotrBody (\s a -> s { _gotrBody = a })

gotrRequestCharged :: Lens' GetObjectTorrentResponse (Maybe RequestCharged)
gotrRequestCharged =
    lens _gotrRequestCharged (\s a -> s { _gotrRequestCharged = a })

instance ToPath GetObjectTorrent where
    toPath GetObjectTorrent{..} = mconcat
        [ "/"
        , toText _gotBucket
        , "/"
        , toText _gotKey
        ]

instance ToQuery GetObjectTorrent where
    toQuery = const "torrent"

instance ToHeaders GetObjectTorrent where
    toHeaders GetObjectTorrent{..} = mconcat
        [ "x-amz-request-payer" =: _gotRequestPayer
        ]

instance ToXMLRoot GetObjectTorrent where
    toXMLRoot = const (namespaced ns "GetObjectTorrent" [])

instance ToXML GetObjectTorrent

instance AWSRequest GetObjectTorrent where
    type Sv GetObjectTorrent = S3
    type Rs GetObjectTorrent = GetObjectTorrentResponse

    request  = get
    response = bodyResponse $ \h s b -> GetObjectTorrentResponse
        <$> pure (RsBody b)
        <*> h ~:? "x-amz-request-charged"
