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

-- Module      : Network.AWS.S3.PutBucketLifecycle
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

-- | Sets lifecycle configuration for your bucket. If a lifecycle configuration
-- exists, it replaces it.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketLifecycle.html>
module Network.AWS.S3.PutBucketLifecycle
    (
    -- * Request
      PutBucketLifecycle
    -- ** Request constructor
    , putBucketLifecycle
    -- ** Request lenses
    , pbl1Bucket
    , pbl1ContentMD5
    , pbl1LifecycleConfiguration

    -- * Response
    , PutBucketLifecycleResponse
    -- ** Response constructor
    , putBucketLifecycleResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketLifecycle = PutBucketLifecycle
    { _pbl1Bucket                 :: Text
    , _pbl1ContentMD5             :: Maybe Text
    , _pbl1LifecycleConfiguration :: Maybe LifecycleConfiguration
    } deriving (Eq, Read, Show)

-- | 'PutBucketLifecycle' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbl1Bucket' @::@ 'Text'
--
-- * 'pbl1ContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbl1LifecycleConfiguration' @::@ 'Maybe' 'LifecycleConfiguration'
--
putBucketLifecycle :: Text -- ^ 'pbl1Bucket'
                   -> PutBucketLifecycle
putBucketLifecycle p1 = PutBucketLifecycle
    { _pbl1Bucket                 = p1
    , _pbl1ContentMD5             = Nothing
    , _pbl1LifecycleConfiguration = Nothing
    }

pbl1Bucket :: Lens' PutBucketLifecycle Text
pbl1Bucket = lens _pbl1Bucket (\s a -> s { _pbl1Bucket = a })

pbl1ContentMD5 :: Lens' PutBucketLifecycle (Maybe Text)
pbl1ContentMD5 = lens _pbl1ContentMD5 (\s a -> s { _pbl1ContentMD5 = a })

pbl1LifecycleConfiguration :: Lens' PutBucketLifecycle (Maybe LifecycleConfiguration)
pbl1LifecycleConfiguration =
    lens _pbl1LifecycleConfiguration
        (\s a -> s { _pbl1LifecycleConfiguration = a })

data PutBucketLifecycleResponse = PutBucketLifecycleResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'PutBucketLifecycleResponse' constructor.
putBucketLifecycleResponse :: PutBucketLifecycleResponse
putBucketLifecycleResponse = PutBucketLifecycleResponse

instance ToPath PutBucketLifecycle where
    toPath PutBucketLifecycle{..} = mconcat
        [ "/"
        , toText _pbl1Bucket
        ]

instance ToQuery PutBucketLifecycle where
    toQuery = const "lifecycle"

instance ToHeaders PutBucketLifecycle where
    toHeaders PutBucketLifecycle{..} = mconcat
        [ "Content-MD5" =: _pbl1ContentMD5
        ]

instance ToXMLRoot PutBucketLifecycle where
    toXMLRoot = extractRoot ns . toXML . _pbl1LifecycleConfiguration

instance ToXML PutBucketLifecycle

instance AWSRequest PutBucketLifecycle where
    type Sv PutBucketLifecycle = S3
    type Rs PutBucketLifecycle = PutBucketLifecycleResponse

    request  = put
    response = nullResponse PutBucketLifecycleResponse
