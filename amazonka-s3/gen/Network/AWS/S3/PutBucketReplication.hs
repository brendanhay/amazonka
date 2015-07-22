{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketReplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new replication configuration (or replaces an existing one, if
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
    , pbrrqContentMD5
    , pbrrqBucket
    , pbrrqReplicationConfiguration

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
-- * 'pbrrqContentMD5'
--
-- * 'pbrrqBucket'
--
-- * 'pbrrqReplicationConfiguration'
data PutBucketReplication = PutBucketReplication'
    { _pbrrqContentMD5               :: !(Maybe Text)
    , _pbrrqBucket                   :: !BucketName
    , _pbrrqReplicationConfiguration :: !ReplicationConfiguration
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'PutBucketReplication' smart constructor.
putBucketReplication :: BucketName -> ReplicationConfiguration -> PutBucketReplication
putBucketReplication pBucket_ pReplicationConfiguration_ =
    PutBucketReplication'
    { _pbrrqContentMD5 = Nothing
    , _pbrrqBucket = pBucket_
    , _pbrrqReplicationConfiguration = pReplicationConfiguration_
    }

-- | FIXME: Undocumented member.
pbrrqContentMD5 :: Lens' PutBucketReplication (Maybe Text)
pbrrqContentMD5 = lens _pbrrqContentMD5 (\ s a -> s{_pbrrqContentMD5 = a});

-- | FIXME: Undocumented member.
pbrrqBucket :: Lens' PutBucketReplication BucketName
pbrrqBucket = lens _pbrrqBucket (\ s a -> s{_pbrrqBucket = a});

-- | FIXME: Undocumented member.
pbrrqReplicationConfiguration :: Lens' PutBucketReplication ReplicationConfiguration
pbrrqReplicationConfiguration = lens _pbrrqReplicationConfiguration (\ s a -> s{_pbrrqReplicationConfiguration = a});

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
              _pbrrqReplicationConfiguration

instance ToHeaders PutBucketReplication where
        toHeaders PutBucketReplication'{..}
          = mconcat ["Content-MD5" =# _pbrrqContentMD5]

instance ToPath PutBucketReplication where
        toPath PutBucketReplication'{..}
          = mconcat ["/", toText _pbrrqBucket]

instance ToQuery PutBucketReplication where
        toQuery = const (mconcat ["replication"])

-- | /See:/ 'putBucketReplicationResponse' smart constructor.
data PutBucketReplicationResponse =
    PutBucketReplicationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketReplicationResponse' smart constructor.
putBucketReplicationResponse :: PutBucketReplicationResponse
putBucketReplicationResponse = PutBucketReplicationResponse'
