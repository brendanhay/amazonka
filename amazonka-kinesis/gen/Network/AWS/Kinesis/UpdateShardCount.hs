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
-- Module      : Network.AWS.Kinesis.UpdateShardCount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the shard count of the specified stream to the specified number of shards.
--
--
-- Updating the shard count is an asynchronous operation. Upon receiving the request, Kinesis Data Streams returns immediately and sets the status of the stream to @UPDATING@ . After the update is complete, Kinesis Data Streams sets the status of the stream back to @ACTIVE@ . Depending on the size of the stream, the scaling action could take a few minutes to complete. You can continue to read and write data to your stream while its status is @UPDATING@ .
--
-- To update the shard count, Kinesis Data Streams performs splits or merges on individual shards. This can cause short-lived shards to be created, in addition to the final shards. We recommend that you double or halve the shard count, as this results in the fewest number of splits or merges.
--
-- This operation has the following limits. You cannot do the following:
--
--     * Scale more than twice per rolling 24-hour period per stream
--
--     * Scale up to more than double your current shard count for a stream
--
--     * Scale down below half your current shard count for a stream
--
--     * Scale up to more than 500 shards in a stream
--
--     * Scale a stream with more than 500 shards down unless the result is less than 500 shards
--
--     * Scale up to more than the shard limit for your account
--
--
--
-- For the default limits for an AWS account, see <http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html Streams Limits> in the /Amazon Kinesis Data Streams Developer Guide/ . To request an increase in the call rate limit, the shard limit for this API, or your overall shard limit, use the <https://console.aws.amazon.com/support/v1#/case/create?issueType=service-limit-increase&amp;limitType=service-code-kinesis limits form> .
--
module Network.AWS.Kinesis.UpdateShardCount
    (
    -- * Creating a Request
      updateShardCount
    , UpdateShardCount
    -- * Request Lenses
    , uscStreamName
    , uscTargetShardCount
    , uscScalingType

    -- * Destructuring the Response
    , updateShardCountResponse
    , UpdateShardCountResponse
    -- * Response Lenses
    , uscrsTargetShardCount
    , uscrsStreamName
    , uscrsCurrentShardCount
    , uscrsResponseStatus
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateShardCount' smart constructor.
data UpdateShardCount = UpdateShardCount'
  { _uscStreamName       :: !Text
  , _uscTargetShardCount :: !Nat
  , _uscScalingType      :: !ScalingType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateShardCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscStreamName' - The name of the stream.
--
-- * 'uscTargetShardCount' - The new number of shards.
--
-- * 'uscScalingType' - The scaling type. Uniform scaling creates shards of equal size.
updateShardCount
    :: Text -- ^ 'uscStreamName'
    -> Natural -- ^ 'uscTargetShardCount'
    -> ScalingType -- ^ 'uscScalingType'
    -> UpdateShardCount
updateShardCount pStreamName_ pTargetShardCount_ pScalingType_ =
  UpdateShardCount'
    { _uscStreamName = pStreamName_
    , _uscTargetShardCount = _Nat # pTargetShardCount_
    , _uscScalingType = pScalingType_
    }


-- | The name of the stream.
uscStreamName :: Lens' UpdateShardCount Text
uscStreamName = lens _uscStreamName (\ s a -> s{_uscStreamName = a})

-- | The new number of shards.
uscTargetShardCount :: Lens' UpdateShardCount Natural
uscTargetShardCount = lens _uscTargetShardCount (\ s a -> s{_uscTargetShardCount = a}) . _Nat

-- | The scaling type. Uniform scaling creates shards of equal size.
uscScalingType :: Lens' UpdateShardCount ScalingType
uscScalingType = lens _uscScalingType (\ s a -> s{_uscScalingType = a})

instance AWSRequest UpdateShardCount where
        type Rs UpdateShardCount = UpdateShardCountResponse
        request = postJSON kinesis
        response
          = receiveJSON
              (\ s h x ->
                 UpdateShardCountResponse' <$>
                   (x .?> "TargetShardCount") <*> (x .?> "StreamName")
                     <*> (x .?> "CurrentShardCount")
                     <*> (pure (fromEnum s)))

instance Hashable UpdateShardCount where

instance NFData UpdateShardCount where

instance ToHeaders UpdateShardCount where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.UpdateShardCount" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateShardCount where
        toJSON UpdateShardCount'{..}
          = object
              (catMaybes
                 [Just ("StreamName" .= _uscStreamName),
                  Just ("TargetShardCount" .= _uscTargetShardCount),
                  Just ("ScalingType" .= _uscScalingType)])

instance ToPath UpdateShardCount where
        toPath = const "/"

instance ToQuery UpdateShardCount where
        toQuery = const mempty

-- | /See:/ 'updateShardCountResponse' smart constructor.
data UpdateShardCountResponse = UpdateShardCountResponse'
  { _uscrsTargetShardCount  :: !(Maybe Nat)
  , _uscrsStreamName        :: !(Maybe Text)
  , _uscrsCurrentShardCount :: !(Maybe Nat)
  , _uscrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateShardCountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscrsTargetShardCount' - The updated number of shards.
--
-- * 'uscrsStreamName' - The name of the stream.
--
-- * 'uscrsCurrentShardCount' - The current number of shards.
--
-- * 'uscrsResponseStatus' - -- | The response status code.
updateShardCountResponse
    :: Int -- ^ 'uscrsResponseStatus'
    -> UpdateShardCountResponse
updateShardCountResponse pResponseStatus_ =
  UpdateShardCountResponse'
    { _uscrsTargetShardCount = Nothing
    , _uscrsStreamName = Nothing
    , _uscrsCurrentShardCount = Nothing
    , _uscrsResponseStatus = pResponseStatus_
    }


-- | The updated number of shards.
uscrsTargetShardCount :: Lens' UpdateShardCountResponse (Maybe Natural)
uscrsTargetShardCount = lens _uscrsTargetShardCount (\ s a -> s{_uscrsTargetShardCount = a}) . mapping _Nat

-- | The name of the stream.
uscrsStreamName :: Lens' UpdateShardCountResponse (Maybe Text)
uscrsStreamName = lens _uscrsStreamName (\ s a -> s{_uscrsStreamName = a})

-- | The current number of shards.
uscrsCurrentShardCount :: Lens' UpdateShardCountResponse (Maybe Natural)
uscrsCurrentShardCount = lens _uscrsCurrentShardCount (\ s a -> s{_uscrsCurrentShardCount = a}) . mapping _Nat

-- | -- | The response status code.
uscrsResponseStatus :: Lens' UpdateShardCountResponse Int
uscrsResponseStatus = lens _uscrsResponseStatus (\ s a -> s{_uscrsResponseStatus = a})

instance NFData UpdateShardCountResponse where
