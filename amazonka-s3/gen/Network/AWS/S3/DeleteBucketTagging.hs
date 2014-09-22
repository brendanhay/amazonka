{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the tags from the bucket.
module Network.AWS.S3.DeleteBucketTagging
    (
    -- * Request
      DeleteBucketTagging
    -- ** Request constructor
    , deleteBucketTagging
    -- ** Request lenses
    , dbtBucket

    -- * Response
    , DeleteBucketTaggingResponse
    -- ** Response constructor
    , deleteBucketTaggingResponse
    ) where

import Network.AWS.Request.RestS3
import Network.AWS.S3.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

newtype DeleteBucketTagging = DeleteBucketTagging
    { _dbtBucket :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketTagging' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @Text@
--
deleteBucketTagging :: Text -- ^ 'dbtBucket'
                    -> DeleteBucketTagging
deleteBucketTagging p1 = DeleteBucketTagging
    { _dbtBucket = p1
    }

dbtBucket :: Lens' DeleteBucketTagging Text
dbtBucket = lens _dbtBucket (\s a -> s { _dbtBucket = a })

instance ToPath DeleteBucketTagging

instance ToQuery DeleteBucketTagging

instance ToHeaders DeleteBucketTagging

instance ToBody DeleteBucketTagging

data DeleteBucketTaggingResponse = DeleteBucketTaggingResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteBucketTaggingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteBucketTaggingResponse :: DeleteBucketTaggingResponse
deleteBucketTaggingResponse = DeleteBucketTaggingResponse

instance AWSRequest DeleteBucketTagging where
    type Sv DeleteBucketTagging = S3
    type Rs DeleteBucketTagging = DeleteBucketTaggingResponse

    request = get
    response _ = nullaryResponse DeleteBucketTaggingResponse
