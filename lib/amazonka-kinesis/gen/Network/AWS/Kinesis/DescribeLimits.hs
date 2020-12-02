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
-- Module      : Network.AWS.Kinesis.DescribeLimits
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the shard limits and usage for the account.
--
--
-- If you update your account limits, the old limits might be returned for a few minutes.
--
-- This operation has a limit of one transaction per second per account.
--
module Network.AWS.Kinesis.DescribeLimits
    (
    -- * Creating a Request
      describeLimits
    , DescribeLimits

    -- * Destructuring the Response
    , describeLimitsResponse
    , DescribeLimitsResponse
    -- * Response Lenses
    , dlrsResponseStatus
    , dlrsShardLimit
    , dlrsOpenShardCount
    ) where

import Network.AWS.Kinesis.Types
import Network.AWS.Kinesis.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLimits' smart constructor.
data DescribeLimits =
  DescribeLimits'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLimits' with the minimum fields required to make a request.
--
describeLimits
    :: DescribeLimits
describeLimits = DescribeLimits'


instance AWSRequest DescribeLimits where
        type Rs DescribeLimits = DescribeLimitsResponse
        request = postJSON kinesis
        response
          = receiveJSON
              (\ s h x ->
                 DescribeLimitsResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "ShardLimit") <*>
                     (x .:> "OpenShardCount"))

instance Hashable DescribeLimits where

instance NFData DescribeLimits where

instance ToHeaders DescribeLimits where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Kinesis_20131202.DescribeLimits" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeLimits where
        toJSON = const (Object mempty)

instance ToPath DescribeLimits where
        toPath = const "/"

instance ToQuery DescribeLimits where
        toQuery = const mempty

-- | /See:/ 'describeLimitsResponse' smart constructor.
data DescribeLimitsResponse = DescribeLimitsResponse'
  { _dlrsResponseStatus :: !Int
  , _dlrsShardLimit     :: !Nat
  , _dlrsOpenShardCount :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeLimitsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlrsResponseStatus' - -- | The response status code.
--
-- * 'dlrsShardLimit' - The maximum number of shards.
--
-- * 'dlrsOpenShardCount' - The number of open shards.
describeLimitsResponse
    :: Int -- ^ 'dlrsResponseStatus'
    -> Natural -- ^ 'dlrsShardLimit'
    -> Natural -- ^ 'dlrsOpenShardCount'
    -> DescribeLimitsResponse
describeLimitsResponse pResponseStatus_ pShardLimit_ pOpenShardCount_ =
  DescribeLimitsResponse'
    { _dlrsResponseStatus = pResponseStatus_
    , _dlrsShardLimit = _Nat # pShardLimit_
    , _dlrsOpenShardCount = _Nat # pOpenShardCount_
    }


-- | -- | The response status code.
dlrsResponseStatus :: Lens' DescribeLimitsResponse Int
dlrsResponseStatus = lens _dlrsResponseStatus (\ s a -> s{_dlrsResponseStatus = a})

-- | The maximum number of shards.
dlrsShardLimit :: Lens' DescribeLimitsResponse Natural
dlrsShardLimit = lens _dlrsShardLimit (\ s a -> s{_dlrsShardLimit = a}) . _Nat

-- | The number of open shards.
dlrsOpenShardCount :: Lens' DescribeLimitsResponse Natural
dlrsOpenShardCount = lens _dlrsOpenShardCount (\ s a -> s{_dlrsOpenShardCount = a}) . _Nat

instance NFData DescribeLimitsResponse where
