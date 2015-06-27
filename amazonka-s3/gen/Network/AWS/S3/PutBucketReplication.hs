{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.PutBucketReplication
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
    , pbrContentMD5
    , pbrBucket
    , pbrReplicationConfiguration

    -- * Response
    , PutBucketReplicationResponse
    -- ** Response constructor
    , putBucketReplicationResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketReplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbrContentMD5'
--
-- * 'pbrBucket'
--
-- * 'pbrReplicationConfiguration'
data PutBucketReplication = PutBucketReplication'
    { _pbrContentMD5               :: Maybe Text
    , _pbrBucket                   :: BucketName
    , _pbrReplicationConfiguration :: ReplicationConfiguration
    } deriving (Eq,Read,Show)

-- | 'PutBucketReplication' smart constructor.
putBucketReplication :: BucketName -> ReplicationConfiguration -> PutBucketReplication
putBucketReplication pBucket pReplicationConfiguration =
    PutBucketReplication'
    { _pbrContentMD5 = Nothing
    , _pbrBucket = pBucket
    , _pbrReplicationConfiguration = pReplicationConfiguration
    }

-- | FIXME: Undocumented member.
pbrContentMD5 :: Lens' PutBucketReplication (Maybe Text)
pbrContentMD5 = lens _pbrContentMD5 (\ s a -> s{_pbrContentMD5 = a});

-- | FIXME: Undocumented member.
pbrBucket :: Lens' PutBucketReplication BucketName
pbrBucket = lens _pbrBucket (\ s a -> s{_pbrBucket = a});

-- | FIXME: Undocumented member.
pbrReplicationConfiguration :: Lens' PutBucketReplication ReplicationConfiguration
pbrReplicationConfiguration = lens _pbrReplicationConfiguration (\ s a -> s{_pbrReplicationConfiguration = a});

instance AWSRequest PutBucketReplication where
        type Sv PutBucketReplication = S3
        type Rs PutBucketReplication =
             PutBucketReplicationResponse
        request = putXML
        response = receiveNull PutBucketReplicationResponse'

instance ToElement PutBucketReplication where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}ReplicationConfiguration"
              .
              _pbrReplicationConfiguration

instance ToHeaders PutBucketReplication where
        toHeaders PutBucketReplication'{..}
          = mconcat ["Content-MD5" =# _pbrContentMD5]

instance ToPath PutBucketReplication where
        toPath PutBucketReplication'{..}
          = mconcat ["/", toText _pbrBucket]

instance ToQuery PutBucketReplication where
        toQuery = const (mconcat ["replication"])

-- | /See:/ 'putBucketReplicationResponse' smart constructor.
data PutBucketReplicationResponse =
    PutBucketReplicationResponse'
    deriving (Eq,Read,Show)

-- | 'PutBucketReplicationResponse' smart constructor.
putBucketReplicationResponse :: PutBucketReplicationResponse
putBucketReplicationResponse = PutBucketReplicationResponse'
