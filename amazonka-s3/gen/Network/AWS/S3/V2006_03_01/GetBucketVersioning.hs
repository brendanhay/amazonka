{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.GetBucketVersioning
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the versioning state of a bucket.
module Network.AWS.S3.V2006_03_01.GetBucketVersioning
    (
    -- * Request
      GetBucketVersioning
    -- ** Request constructor
    , getBucketVersioning
    -- ** Request lenses
    , gbvrBucket

    -- * Response
    , GetBucketVersioningResponse
    -- ** Response lenses
    , gbvoStatus
    , gbvoMfaDelete
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetBucketVersioning' request.
getBucketVersioning :: BucketName -- ^ 'gbvrBucket'
                    -> GetBucketVersioning
getBucketVersioning p1 = GetBucketVersioning
    { _gbvrBucket = p1
    }

data GetBucketVersioning = GetBucketVersioning
    { _gbvrBucket :: BucketName
    } deriving (Show, Generic)

gbvrBucket
    :: Functor f
    => (BucketName
    -> f (BucketName))
    -> GetBucketVersioning
    -> f GetBucketVersioning
gbvrBucket f x =
    (\y -> x { _gbvrBucket = y })
       <$> f (_gbvrBucket x)
{-# INLINE gbvrBucket #-}

instance ToPath GetBucketVersioning where
    toPath GetBucketVersioning{..} = mconcat
        [ "/"
        , toBS _gbvrBucket
        ]

instance ToQuery GetBucketVersioning where
    toQuery GetBucketVersioning{..} = mconcat
        [ "versioning"
        ]

instance ToHeaders GetBucketVersioning

instance ToBody GetBucketVersioning

data GetBucketVersioningResponse = GetBucketVersioningResponse
    { _gbvoStatus :: Maybe (Switch BucketVersioningStatus)
      -- ^ The versioning state of the bucket.
    , _gbvoMfaDelete :: Maybe (Switch MFADeleteStatus)
      -- ^ Specifies whether MFA delete is enabled in the bucket versioning
      -- configuration. This element is only returned if the bucket has
      -- been configured with MFA delete. If the bucket has never been so
      -- configured, this element is not returned.
    } deriving (Show, Generic)

-- | The versioning state of the bucket.
gbvoStatus
    :: Functor f
    => (Maybe (Switch BucketVersioningStatus)
    -> f (Maybe (Switch BucketVersioningStatus)))
    -> GetBucketVersioningResponse
    -> f GetBucketVersioningResponse
gbvoStatus f x =
    (\y -> x { _gbvoStatus = y })
       <$> f (_gbvoStatus x)
{-# INLINE gbvoStatus #-}

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
gbvoMfaDelete
    :: Functor f
    => (Maybe (Switch MFADeleteStatus)
    -> f (Maybe (Switch MFADeleteStatus)))
    -> GetBucketVersioningResponse
    -> f GetBucketVersioningResponse
gbvoMfaDelete f x =
    (\y -> x { _gbvoMfaDelete = y })
       <$> f (_gbvoMfaDelete x)
{-# INLINE gbvoMfaDelete #-}

instance FromXML GetBucketVersioningResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketVersioning where
    type Sv GetBucketVersioning = S3
    type Rs GetBucketVersioning = GetBucketVersioningResponse

    request = get
    response _ = xmlResponse
