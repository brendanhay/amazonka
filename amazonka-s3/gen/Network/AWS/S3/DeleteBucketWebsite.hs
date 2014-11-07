{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketWebsite
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation removes the website configuration from the bucket.
module Network.AWS.S3.DeleteBucketWebsite
    (
    -- * Request
      DeleteBucketWebsite
    -- ** Request constructor
    , deleteBucketWebsite
    -- ** Request lenses
    , dbwrBucket

    -- * Response
    , DeleteBucketWebsiteResponse
    -- ** Response constructor
    , deleteBucketWebsiteResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype DeleteBucketWebsite = DeleteBucketWebsite
    { _dbwrBucket :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteBucketWebsite' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbwrBucket' @::@ 'Text'
--
deleteBucketWebsite :: Text -- ^ 'dbwrBucket'
                    -> DeleteBucketWebsite
deleteBucketWebsite p1 = DeleteBucketWebsite
    { _dbwrBucket = p1
    }

dbwrBucket :: Lens' DeleteBucketWebsite Text
dbwrBucket = lens _dbwrBucket (\s a -> s { _dbwrBucket = a })

instance ToPath DeleteBucketWebsite where
    toPath DeleteBucketWebsite{..} = mconcat
        [ "/"
        , toText _dbwrBucket
        ]

instance ToQuery DeleteBucketWebsite where
    toQuery = const "website"

instance ToHeaders DeleteBucketWebsite

data DeleteBucketWebsiteResponse = DeleteBucketWebsiteResponse
-- | 'DeleteBucketWebsiteResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
deleteBucketWebsiteResponse :: DeleteBucketWebsiteResponse
deleteBucketWebsiteResponse = DeleteBucketWebsiteResponse

instance AWSRequest DeleteBucketWebsite where
    type Sv DeleteBucketWebsite = S3
    type Rs DeleteBucketWebsite = DeleteBucketWebsiteResponse

    request  = delete'
    response = const (nullaryResponse DeleteBucketWebsiteResponse)
