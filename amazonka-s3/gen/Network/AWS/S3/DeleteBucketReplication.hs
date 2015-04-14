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

-- Module      : Network.AWS.S3.DeleteBucketReplication
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

-- | <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketReplication.html>
module Network.AWS.S3.DeleteBucketReplication
    (
    -- * Request
      DeleteBucketReplication
    -- ** Request constructor
    , deleteBucketReplication
    -- ** Request lenses
    , dbrBucket

    -- * Response
    , DeleteBucketReplicationResponse
    -- ** Response constructor
    , deleteBucketReplicationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype DeleteBucketReplication = DeleteBucketReplication
    { _dbrBucket :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteBucketReplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbrBucket' @::@ 'Text'
--
deleteBucketReplication :: Text -- ^ 'dbrBucket'
                        -> DeleteBucketReplication
deleteBucketReplication p1 = DeleteBucketReplication
    { _dbrBucket = p1
    }

dbrBucket :: Lens' DeleteBucketReplication Text
dbrBucket = lens _dbrBucket (\s a -> s { _dbrBucket = a })

data DeleteBucketReplicationResponse = DeleteBucketReplicationResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteBucketReplicationResponse' constructor.
deleteBucketReplicationResponse :: DeleteBucketReplicationResponse
deleteBucketReplicationResponse = DeleteBucketReplicationResponse

instance ToPath DeleteBucketReplication where
    toPath DeleteBucketReplication{..} = mconcat
        [ "/"
        , toText _dbrBucket
        ]

instance ToQuery DeleteBucketReplication where
    toQuery = const "replication"

instance ToHeaders DeleteBucketReplication

instance ToXMLRoot DeleteBucketReplication where
    toXMLRoot = const (namespaced ns "DeleteBucketReplication" [])

instance ToXML DeleteBucketReplication

instance AWSRequest DeleteBucketReplication where
    type Sv DeleteBucketReplication = S3
    type Rs DeleteBucketReplication = DeleteBucketReplicationResponse

    request  = delete
    response = nullResponse DeleteBucketReplicationResponse
