{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

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
    { _gbvrBucket :: Text
    } deriving ()

-- | 'GetBucketVersioning' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbvrBucket' @::@ 'Text'
--
getBucketVersioning :: Text -- ^ 'gbvrBucket'
                    -> GetBucketVersioning
getBucketVersioning p1 = GetBucketVersioning
    { _gbvrBucket = p1
    }

gbvrBucket :: Lens' GetBucketVersioning Text
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

-- | 'GetBucketVersioningOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbvoMFADelete' @::@ 'Maybe' 'Text'
--
-- * 'gbvoStatus' @::@ 'Maybe' 'Text'
--
getBucketVersioningOutput :: GetBucketVersioningOutput
getBucketVersioningOutput = GetBucketVersioningOutput
    { _gbvoStatus    = Nothing
    , _gbvoMFADelete = Nothing
    }

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
gbvoMFADelete :: Lens' GetBucketVersioningOutput (Maybe Text)
gbvoMFADelete = lens _gbvoMFADelete (\s a -> s { _gbvoMFADelete = a })

-- | The versioning state of the bucket.
gbvoStatus :: Lens' GetBucketVersioningOutput (Maybe Text)
gbvoStatus = lens _gbvoStatus (\s a -> s { _gbvoStatus = a })

instance AWSRequest GetBucketVersioning where
    type Sv GetBucketVersioning = S3
    type Rs GetBucketVersioning = GetBucketVersioningOutput

    request  = get'
    response = const . xmlResponse $ \h x -> GetBucketVersioningOutput
        <$> x %| "MfaDelete"
        <*> x %| "Status"
