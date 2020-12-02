{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketReplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new replication configuration (or replaces an existing one, if present).
module Network.AWS.S3.PutBucketReplication
    (
    -- * Creating a Request
      putBucketReplication
    , PutBucketReplication
    -- * Request Lenses
    , pbrContentMD5
    , pbrBucket
    , pbrReplicationConfiguration

    -- * Destructuring the Response
    , putBucketReplicationResponse
    , PutBucketReplicationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'putBucketReplication' smart constructor.
data PutBucketReplication = PutBucketReplication'
  { _pbrContentMD5               :: !(Maybe Text)
  , _pbrBucket                   :: !BucketName
  , _pbrReplicationConfiguration :: !ReplicationConfiguration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketReplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbrContentMD5' - Undocumented member.
--
-- * 'pbrBucket' - Undocumented member.
--
-- * 'pbrReplicationConfiguration' - Undocumented member.
putBucketReplication
    :: BucketName -- ^ 'pbrBucket'
    -> ReplicationConfiguration -- ^ 'pbrReplicationConfiguration'
    -> PutBucketReplication
putBucketReplication pBucket_ pReplicationConfiguration_ =
  PutBucketReplication'
    { _pbrContentMD5 = Nothing
    , _pbrBucket = pBucket_
    , _pbrReplicationConfiguration = pReplicationConfiguration_
    }


-- | Undocumented member.
pbrContentMD5 :: Lens' PutBucketReplication (Maybe Text)
pbrContentMD5 = lens _pbrContentMD5 (\ s a -> s{_pbrContentMD5 = a})

-- | Undocumented member.
pbrBucket :: Lens' PutBucketReplication BucketName
pbrBucket = lens _pbrBucket (\ s a -> s{_pbrBucket = a})

-- | Undocumented member.
pbrReplicationConfiguration :: Lens' PutBucketReplication ReplicationConfiguration
pbrReplicationConfiguration = lens _pbrReplicationConfiguration (\ s a -> s{_pbrReplicationConfiguration = a})

instance AWSRequest PutBucketReplication where
        type Rs PutBucketReplication =
             PutBucketReplicationResponse
        request = putXML s3
        response = receiveNull PutBucketReplicationResponse'

instance Hashable PutBucketReplication where

instance NFData PutBucketReplication where

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
          = mconcat ["/", toBS _pbrBucket]

instance ToQuery PutBucketReplication where
        toQuery = const (mconcat ["replication"])

-- | /See:/ 'putBucketReplicationResponse' smart constructor.
data PutBucketReplicationResponse =
  PutBucketReplicationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutBucketReplicationResponse' with the minimum fields required to make a request.
--
putBucketReplicationResponse
    :: PutBucketReplicationResponse
putBucketReplicationResponse = PutBucketReplicationResponse'


instance NFData PutBucketReplicationResponse where
