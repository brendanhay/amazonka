{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.S3.V2006_03_01.GetBucketVersioning where

import Control.Lens
import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

data GetBucketVersioning = GetBucketVersioning
    { _gbvrBucket :: BucketName
    } deriving (Generic)

makeLenses ''GetBucketVersioning

instance ToPath GetBucketVersioning where
    toPath GetBucketVersioning{..} = mconcat
        [ "/"
        , toBS _gbvrBucket
        ]

instance ToQuery GetBucketVersioning

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
    } deriving (Generic)

makeLenses ''GetBucketVersioningResponse

instance FromXML GetBucketVersioningResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetBucketVersioning where
    type Sv GetBucketVersioning = S3
    type Rs GetBucketVersioning = GetBucketVersioningResponse

    request = get
    response _ = xmlResponse
