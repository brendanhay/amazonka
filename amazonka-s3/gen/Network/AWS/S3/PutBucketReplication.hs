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

-- Module      : Network.AWS.S3.PutBucketReplication
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

-- | Creates a new replication configuration (or replaces an existing one, if
-- present).
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketReplication.html>
module Network.AWS.S3.PutBucketReplication
    (
    -- * Request
      PutBucketReplication
    -- ** Request constructor
    , putBucketReplication
    -- ** Request lenses
    , pbrBucket
    , pbrContentMD5
    , pbrReplicationConfiguration

    -- * Response
    , PutBucketReplicationResponse
    -- ** Response constructor
    , putBucketReplicationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

data PutBucketReplication = PutBucketReplication
    { _pbrBucket                   :: Text
    , _pbrContentMD5               :: Maybe Text
    , _pbrReplicationConfiguration :: ReplicationConfiguration
    } deriving (Eq, Read, Show)

-- | 'PutBucketReplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbrBucket' @::@ 'Text'
--
-- * 'pbrContentMD5' @::@ 'Maybe' 'Text'
--
-- * 'pbrReplicationConfiguration' @::@ 'ReplicationConfiguration'
--
putBucketReplication :: Text -- ^ 'pbrBucket'
                     -> ReplicationConfiguration -- ^ 'pbrReplicationConfiguration'
                     -> PutBucketReplication
putBucketReplication p1 p2 = PutBucketReplication
    { _pbrBucket                   = p1
    , _pbrReplicationConfiguration = p2
    , _pbrContentMD5               = Nothing
    }

pbrBucket :: Lens' PutBucketReplication Text
pbrBucket = lens _pbrBucket (\s a -> s { _pbrBucket = a })

pbrContentMD5 :: Lens' PutBucketReplication (Maybe Text)
pbrContentMD5 = lens _pbrContentMD5 (\s a -> s { _pbrContentMD5 = a })

pbrReplicationConfiguration :: Lens' PutBucketReplication ReplicationConfiguration
pbrReplicationConfiguration =
    lens _pbrReplicationConfiguration
        (\s a -> s { _pbrReplicationConfiguration = a })

data PutBucketReplicationResponse = PutBucketReplicationResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'PutBucketReplicationResponse' constructor.
putBucketReplicationResponse :: PutBucketReplicationResponse
putBucketReplicationResponse = PutBucketReplicationResponse

instance ToPath PutBucketReplication where
    toPath PutBucketReplication{..} = mconcat
        [ "/"
        , toText _pbrBucket
        ]

instance ToQuery PutBucketReplication where
    toQuery = const "replication"

instance ToHeaders PutBucketReplication where
    toHeaders PutBucketReplication{..} = mconcat
        [ "Content-MD5" =: _pbrContentMD5
        ]

instance ToXMLRoot PutBucketReplication where
    toXMLRoot = extractRoot ns . toXML . _pbrReplicationConfiguration

instance ToXML PutBucketReplication

instance AWSRequest PutBucketReplication where
    type Sv PutBucketReplication = S3
    type Rs PutBucketReplication = PutBucketReplicationResponse

    request  = put
    response = nullResponse PutBucketReplicationResponse
