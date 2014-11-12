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

-- Module      : Network.AWS.S3.PutBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets lifecycle configuration for your bucket. If a lifecycle configuration
-- exists, it replaces it.
module Network.AWS.S3.PutBucketLifecycle
    (
    -- * Request
      PutBucketLifecycle
    -- ** Request constructor
    , putBucketLifecycle
    -- ** Request lenses
    , pblBucket
    , pblContentMD5
    , pblLifecycleConfiguration

    -- * Response
    , PutBucketLifecycleResponse
    -- ** Response constructor
    , putBucketLifecycleResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data PutBucketLifecycle = PutBucketLifecycle
    { _pblBucket                 :: Text
    , _pblContentMD5             :: Maybe Text
    , _pblLifecycleConfiguration :: Maybe LifecycleConfiguration
    } (Eq, Show, Generic)

-- | 'PutBucketLifecycle' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pblBucket' @::@ 'Text'
--
-- * 'pblContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pblLifecycleConfiguration' @::@ 'Maybe' 'LifecycleConfiguration'
--
putBucketLifecycle :: Text -- ^ 'pblBucket'
                   -> PutBucketLifecycle
putBucketLifecycle p1 = PutBucketLifecycle
    { _pblBucket                 = p1
    , _pblContentMD5             = Nothing
    , _pblLifecycleConfiguration = Nothing
    }

pblBucket :: Lens' PutBucketLifecycle Text
pblBucket = lens _pblBucket (\s a -> s { _pblBucket = a })

pblContentMD5 :: Lens' PutBucketLifecycle (Maybe Text)
pblContentMD5 = lens _pblContentMD5 (\s a -> s { _pblContentMD5 = a })

pblLifecycleConfiguration :: Lens' PutBucketLifecycle (Maybe LifecycleConfiguration)
pblLifecycleConfiguration =
    lens _pblLifecycleConfiguration
        (\s a -> s { _pblLifecycleConfiguration = a })

instance ToPath PutBucketLifecycle where
    toPath PutBucketLifecycle{..} = mconcat
        [ "/"
        , toText _pblBucket
        ]

instance ToQuery PutBucketLifecycle where
    toQuery = const "lifecycle"

instance ToHeaders PutBucketLifecycle where
    toHeaders PutBucketLifecycle{..} = mconcat
        [ "Content-MD5" =: _pblContentMD5
        ]

instance ToBody PutBucketLifecycle where
    toBody = toBody . encodeXML . _pblLifecycleConfiguration

data PutBucketLifecycleResponse = PutBucketLifecycleResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutBucketLifecycleResponse' constructor.
putBucketLifecycleResponse :: PutBucketLifecycleResponse
putBucketLifecycleResponse = PutBucketLifecycleResponse

instance FromXML PutBucketLifecycleResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PutBucketLifecycleResponse"
instance AWSRequest PutBucketLifecycle where
    type Sv PutBucketLifecycle = S3
    type Rs PutBucketLifecycle = PutBucketLifecycleResponse

    request  = put
    response = nullaryResponse PutBucketLifecycleResponse
