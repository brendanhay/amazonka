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

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

newtype DeleteBucketTagging = DeleteBucketTagging
    { _dbtBucket :: Text
    } (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteBucketTagging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbtBucket' @::@ 'Text'
--
deleteBucketTagging :: Text -- ^ 'dbtBucket'
                    -> DeleteBucketTagging
deleteBucketTagging p1 = DeleteBucketTagging
    { _dbtBucket = p1
    }

dbtBucket :: Lens' DeleteBucketTagging Text
dbtBucket = lens _dbtBucket (\s a -> s { _dbtBucket = a })

instance ToPath DeleteBucketTagging where
    toPath DeleteBucketTagging{..} = mconcat
        [ "/"
        , toText _dbtBucket
        ]

instance ToQuery DeleteBucketTagging where
    toQuery = const "tagging"

instance ToHeaders DeleteBucketTagging

data DeleteBucketTaggingResponse = DeleteBucketTaggingResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteBucketTaggingResponse' constructor.
deleteBucketTaggingResponse :: DeleteBucketTaggingResponse
deleteBucketTaggingResponse = DeleteBucketTaggingResponse

instance FromXML DeleteBucketTaggingResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteBucketTaggingResponse"
instance AWSRequest DeleteBucketTagging where
    type Sv DeleteBucketTagging = S3
    type Rs DeleteBucketTagging = DeleteBucketTaggingResponse

    request  = delete
    response = nullaryResponse DeleteBucketTaggingResponse
