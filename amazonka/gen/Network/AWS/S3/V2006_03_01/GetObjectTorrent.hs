{-# LANGUAGE DeriveGeneric               #-}
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
module Network.AWS.S3.V2006_03_01.GetObjectTorrent where

import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

-- | Smart constructor utilising default fields to
-- specify the minimum viable GetObjectTorrent request.
getObjectTorrent :: BucketName -- ^ 'gotrBucket'
                 -> ObjectKey -- ^ 'gotrKey'
                 -> GetObjectTorrent
getObjectTorrent p1 p2 = GetObjectTorrent
    { gotrBucket = p1
    , gotrKey = p2
    }

data GetObjectTorrent = GetObjectTorrent
    { gotrBucket :: BucketName
    , gotrKey :: ObjectKey
    } deriving (Eq, Show, Generic)

instance ToPath GetObjectTorrent where
    toPath GetObjectTorrent{..} = mconcat
        [ "/"
        , toBS gotrBucket
        , "/"
        , toBS gotrKey
        ]

instance ToQuery GetObjectTorrent

instance ToHeaders GetObjectTorrent

instance ToBody GetObjectTorrent

instance AWSRequest GetObjectTorrent where
    type Sv GetObjectTorrent = S3

    request  = get
    response = response' $

data instance Rs GetObjectTorrent = GetObjectTorrentResponse
    { gotoBody :: Maybe ByteString
    } deriving (Eq, Show, Generic)
