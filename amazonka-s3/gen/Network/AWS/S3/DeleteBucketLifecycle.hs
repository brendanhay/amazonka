{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketLifecycle
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the lifecycle configuration from the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketLifecycle.html>
module Network.AWS.S3.DeleteBucketLifecycle
    (
    -- * Request
      DeleteBucketLifecycle
    -- ** Request constructor
    , deleteBucketLifecycle
    -- ** Request lenses
    , dblBucket

    -- * Response
    , DeleteBucketLifecycleResponse
    -- ** Response constructor
    , deleteBucketLifecycleResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.XML
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype DeleteBucketLifecycle = DeleteBucketLifecycle
    { _dblBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteBucketLifecycle' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dblBucket' @::@ 'Text'
--
deleteBucketLifecycle :: Text -- ^ 'dblBucket'
                      -> DeleteBucketLifecycle
deleteBucketLifecycle p1 = DeleteBucketLifecycle
    { _dblBucket = p1
    }

dblBucket :: Lens' DeleteBucketLifecycle Text
dblBucket = lens _dblBucket (\s a -> s { _dblBucket = a })

data DeleteBucketLifecycleResponse = DeleteBucketLifecycleResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteBucketLifecycleResponse' constructor.
deleteBucketLifecycleResponse :: DeleteBucketLifecycleResponse
deleteBucketLifecycleResponse = DeleteBucketLifecycleResponse

instance AWSRequest DeleteBucketLifecycle where
    type Sv DeleteBucketLifecycle = S3
    type Rs DeleteBucketLifecycle = DeleteBucketLifecycleResponse

    request  = delete
    response = nullResponse DeleteBucketLifecycleResponse

instance ToPath DeleteBucketLifecycle where
    toPath DeleteBucketLifecycle{..} = mconcat
        [ "/"
        , toText _dblBucket
        ]

instance ToHeaders DeleteBucketLifecycle

instance ToQuery DeleteBucketLifecycle where
    toQuery = const "lifecycle"

instance ToXML DeleteBucketLifecycle where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "DeleteBucketLifecycle"
