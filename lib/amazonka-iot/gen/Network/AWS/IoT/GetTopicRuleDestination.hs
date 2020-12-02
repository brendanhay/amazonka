{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a topic rule destination.
module Network.AWS.IoT.GetTopicRuleDestination
  ( -- * Creating a Request
    getTopicRuleDestination,
    GetTopicRuleDestination,

    -- * Request Lenses
    gtrdArn,

    -- * Destructuring the Response
    getTopicRuleDestinationResponse,
    GetTopicRuleDestinationResponse,

    -- * Response Lenses
    gtrdrsTopicRuleDestination,
    gtrdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTopicRuleDestination' smart constructor.
newtype GetTopicRuleDestination = GetTopicRuleDestination'
  { _gtrdArn ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTopicRuleDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrdArn' - The ARN of the topic rule destination.
getTopicRuleDestination ::
  -- | 'gtrdArn'
  Text ->
  GetTopicRuleDestination
getTopicRuleDestination pArn_ =
  GetTopicRuleDestination' {_gtrdArn = pArn_}

-- | The ARN of the topic rule destination.
gtrdArn :: Lens' GetTopicRuleDestination Text
gtrdArn = lens _gtrdArn (\s a -> s {_gtrdArn = a})

instance AWSRequest GetTopicRuleDestination where
  type Rs GetTopicRuleDestination = GetTopicRuleDestinationResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          GetTopicRuleDestinationResponse'
            <$> (x .?> "topicRuleDestination") <*> (pure (fromEnum s))
      )

instance Hashable GetTopicRuleDestination

instance NFData GetTopicRuleDestination

instance ToHeaders GetTopicRuleDestination where
  toHeaders = const mempty

instance ToPath GetTopicRuleDestination where
  toPath GetTopicRuleDestination' {..} =
    mconcat ["/destinations/", toBS _gtrdArn]

instance ToQuery GetTopicRuleDestination where
  toQuery = const mempty

-- | /See:/ 'getTopicRuleDestinationResponse' smart constructor.
data GetTopicRuleDestinationResponse = GetTopicRuleDestinationResponse'
  { _gtrdrsTopicRuleDestination ::
      !( Maybe
           TopicRuleDestination
       ),
    _gtrdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTopicRuleDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtrdrsTopicRuleDestination' - The topic rule destination.
--
-- * 'gtrdrsResponseStatus' - -- | The response status code.
getTopicRuleDestinationResponse ::
  -- | 'gtrdrsResponseStatus'
  Int ->
  GetTopicRuleDestinationResponse
getTopicRuleDestinationResponse pResponseStatus_ =
  GetTopicRuleDestinationResponse'
    { _gtrdrsTopicRuleDestination =
        Nothing,
      _gtrdrsResponseStatus = pResponseStatus_
    }

-- | The topic rule destination.
gtrdrsTopicRuleDestination :: Lens' GetTopicRuleDestinationResponse (Maybe TopicRuleDestination)
gtrdrsTopicRuleDestination = lens _gtrdrsTopicRuleDestination (\s a -> s {_gtrdrsTopicRuleDestination = a})

-- | -- | The response status code.
gtrdrsResponseStatus :: Lens' GetTopicRuleDestinationResponse Int
gtrdrsResponseStatus = lens _gtrdrsResponseStatus (\s a -> s {_gtrdrsResponseStatus = a})

instance NFData GetTopicRuleDestinationResponse
