{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the versioning state of a bucket.
module Network.AWS.S3.GetBucketVersioning
    (
    -- * Request
      GetBucketVersioning
    -- ** Request constructor
    , getBucketVersioning
    -- ** Request lenses
    , gbvrBucket

    -- * Response
    , GetBucketVersioningOutput
    -- ** Response constructor
    , getBucketVersioningOutput
    -- ** Response lenses
    , gbvoMFADelete
    , gbvoStatus
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype GetBucketVersioning = GetBucketVersioning
    { _gbvrBucket :: BucketName
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetBucketVersioning' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbvrBucket' @::@ 'BucketName'
--
getBucketVersioning :: BucketName -- ^ 'gbvrBucket'
                    -> GetBucketVersioning
getBucketVersioning p1 = GetBucketVersioning
    { _gbvrBucket = p1
    }

gbvrBucket :: Lens' GetBucketVersioning BucketName
gbvrBucket = lens _gbvrBucket (\s a -> s { _gbvrBucket = a })

instance ToPath GetBucketVersioning where
    toPath GetBucketVersioning{..} = mconcat
        [ "/"
        , toText _gbvrBucket
        ]

instance ToQuery GetBucketVersioning where
    toQuery = const "versioning"

instance ToHeaders GetBucketVersioning

data GetBucketVersioningOutput = GetBucketVersioningOutput
    { _gbvoMFADelete :: Maybe Text
    , _gbvoStatus    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest GetBucketVersioning where
    type Sv GetBucketVersioning = S3
    type Rs GetBucketVersioning = GetBucketVersioningOutput

    request  = get
    response = const . xmlResponse $ \h x ->
        <$> x %| "MfaDelete"
        <*> x %| "Status"
