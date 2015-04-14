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

-- Module      : Network.AWS.S3.AbortMultipartUpload
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

-- | Aborts a multipart upload.
--
-- To verify that all parts have been removed, so you don't get charged for the
-- part storage, you should call the List Parts operation and ensure the parts
-- list is empty.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/AbortMultipartUpload.html>
module Network.AWS.S3.AbortMultipartUpload
    (
    -- * Request
      AbortMultipartUpload
    -- ** Request constructor
    , abortMultipartUpload
    -- ** Request lenses
    , amuBucket
    , amuKey
    , amuRequestPayer
    , amuUploadId

    -- * Response
    , AbortMultipartUploadResponse
    -- ** Response constructor
    , abortMultipartUploadResponse
    -- ** Response lenses
    , amurRequestCharged
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data AbortMultipartUpload = AbortMultipartUpload
    { _amuBucket       :: Text
    , _amuKey          :: Text
    , _amuRequestPayer :: Maybe RequestPayer
    , _amuUploadId     :: Text
    } deriving (Eq, Read, Show)

-- | 'AbortMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amuBucket' @::@ 'Text'
--
-- * 'amuKey' @::@ 'Text'
--
-- * 'amuRequestPayer' @::@ 'Maybe' 'RequestPayer'
--
-- * 'amuUploadId' @::@ 'Text'
--
abortMultipartUpload :: Text -- ^ 'amuBucket'
                     -> Text -- ^ 'amuKey'
                     -> Text -- ^ 'amuUploadId'
                     -> AbortMultipartUpload
abortMultipartUpload p1 p2 p3 = AbortMultipartUpload
    { _amuBucket       = p1
    , _amuKey          = p2
    , _amuUploadId     = p3
    , _amuRequestPayer = Nothing
    }

amuBucket :: Lens' AbortMultipartUpload Text
amuBucket = lens _amuBucket (\s a -> s { _amuBucket = a })

amuKey :: Lens' AbortMultipartUpload Text
amuKey = lens _amuKey (\s a -> s { _amuKey = a })

amuRequestPayer :: Lens' AbortMultipartUpload (Maybe RequestPayer)
amuRequestPayer = lens _amuRequestPayer (\s a -> s { _amuRequestPayer = a })

amuUploadId :: Lens' AbortMultipartUpload Text
amuUploadId = lens _amuUploadId (\s a -> s { _amuUploadId = a })

newtype AbortMultipartUploadResponse = AbortMultipartUploadResponse
    { _amurRequestCharged :: Maybe RequestCharged
    } deriving (Eq, Read, Show)

-- | 'AbortMultipartUploadResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'amurRequestCharged' @::@ 'Maybe' 'RequestCharged'
--
abortMultipartUploadResponse :: AbortMultipartUploadResponse
abortMultipartUploadResponse = AbortMultipartUploadResponse
    { _amurRequestCharged = Nothing
    }

amurRequestCharged :: Lens' AbortMultipartUploadResponse (Maybe RequestCharged)
amurRequestCharged =
    lens _amurRequestCharged (\s a -> s { _amurRequestCharged = a })

instance ToPath AbortMultipartUpload where
    toPath AbortMultipartUpload{..} = mconcat
        [ "/"
        , toText _amuBucket
        , "/"
        , toText _amuKey
        ]

instance ToQuery AbortMultipartUpload where
    toQuery rq = "uploadId" =? _amuUploadId rq

instance ToHeaders AbortMultipartUpload where
    toHeaders AbortMultipartUpload{..} = mconcat
        [ "x-amz-request-payer" =: _amuRequestPayer
        ]

instance ToXMLRoot AbortMultipartUpload where
    toXMLRoot = const (namespaced ns "AbortMultipartUpload" [])

instance ToXML AbortMultipartUpload

instance AWSRequest AbortMultipartUpload where
    type Sv AbortMultipartUpload = S3
    type Rs AbortMultipartUpload = AbortMultipartUploadResponse

    request  = delete
    response = headerResponse $ \h -> AbortMultipartUploadResponse
        <$> h ~:? "x-amz-request-charged"
