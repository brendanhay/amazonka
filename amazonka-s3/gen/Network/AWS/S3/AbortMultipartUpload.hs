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

-- Module      : Network.AWS.S3.AbortMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Aborts a multipart upload. To verify that all parts have been removed, so
-- you don't get charged for the part storage, you should call the List Parts
-- operation and ensure the parts list is empty.
module Network.AWS.S3.AbortMultipartUpload
    (
    -- * Request
      AbortMultipartUpload
    -- ** Request constructor
    , abortMultipartUpload
    -- ** Request lenses
    , amuBucket
    , amuKey
    , amuUploadId

    -- * Response
    , AbortMultipartUploadResponse
    -- ** Response constructor
    , abortMultipartUploadResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data AbortMultipartUpload = AbortMultipartUpload
    { _amuBucket   :: Text
    , _amuKey      :: Text
    , _amuUploadId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AbortMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amuBucket' @::@ 'Text'
--
-- * 'amuKey' @::@ 'Text'
--
-- * 'amuUploadId' @::@ 'Text'
--
abortMultipartUpload :: Text -- ^ 'amuBucket'
                     -> Text -- ^ 'amuKey'
                     -> Text -- ^ 'amuUploadId'
                     -> AbortMultipartUpload
abortMultipartUpload p1 p2 p3 = AbortMultipartUpload
    { _amuBucket   = p1
    , _amuKey      = p2
    , _amuUploadId = p3
    }

amuBucket :: Lens' AbortMultipartUpload Text
amuBucket = lens _amuBucket (\s a -> s { _amuBucket = a })

amuKey :: Lens' AbortMultipartUpload Text
amuKey = lens _amuKey (\s a -> s { _amuKey = a })

amuUploadId :: Lens' AbortMultipartUpload Text
amuUploadId = lens _amuUploadId (\s a -> s { _amuUploadId = a })

instance ToPath AbortMultipartUpload where
    toPath AbortMultipartUpload{..} = mconcat
        [ "/"
        , toText _amuBucket
        , "/"
        , toText _amuKey
        ]

instance ToQuery AbortMultipartUpload where
    toQuery x = "uploadId" =? _amuUploadId x

instance ToHeaders AbortMultipartUpload

data AbortMultipartUploadResponse = AbortMultipartUploadResponse

-- | 'AbortMultipartUploadResponse' constructor.
abortMultipartUploadResponse :: AbortMultipartUploadResponse
abortMultipartUploadResponse = AbortMultipartUploadResponse

instance AWSRequest AbortMultipartUpload where
    type Sv AbortMultipartUpload = S3
    type Rs AbortMultipartUpload = AbortMultipartUploadResponse

    request  = delete
    response = const (nullaryResponse AbortMultipartUploadResponse)
