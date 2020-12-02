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
-- Module      : Network.AWS.IoT.DeleteTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a topic rule destination.
module Network.AWS.IoT.DeleteTopicRuleDestination
  ( -- * Creating a Request
    deleteTopicRuleDestination,
    DeleteTopicRuleDestination,

    -- * Request Lenses
    dtrdArn,

    -- * Destructuring the Response
    deleteTopicRuleDestinationResponse,
    DeleteTopicRuleDestinationResponse,

    -- * Response Lenses
    dtrdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTopicRuleDestination' smart constructor.
newtype DeleteTopicRuleDestination = DeleteTopicRuleDestination'
  { _dtrdArn ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTopicRuleDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrdArn' - The ARN of the topic rule destination to delete.
deleteTopicRuleDestination ::
  -- | 'dtrdArn'
  Text ->
  DeleteTopicRuleDestination
deleteTopicRuleDestination pArn_ =
  DeleteTopicRuleDestination' {_dtrdArn = pArn_}

-- | The ARN of the topic rule destination to delete.
dtrdArn :: Lens' DeleteTopicRuleDestination Text
dtrdArn = lens _dtrdArn (\s a -> s {_dtrdArn = a})

instance AWSRequest DeleteTopicRuleDestination where
  type
    Rs DeleteTopicRuleDestination =
      DeleteTopicRuleDestinationResponse
  request = delete ioT
  response =
    receiveEmpty
      ( \s h x ->
          DeleteTopicRuleDestinationResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteTopicRuleDestination

instance NFData DeleteTopicRuleDestination

instance ToHeaders DeleteTopicRuleDestination where
  toHeaders = const mempty

instance ToPath DeleteTopicRuleDestination where
  toPath DeleteTopicRuleDestination' {..} =
    mconcat ["/destinations/", toBS _dtrdArn]

instance ToQuery DeleteTopicRuleDestination where
  toQuery = const mempty

-- | /See:/ 'deleteTopicRuleDestinationResponse' smart constructor.
newtype DeleteTopicRuleDestinationResponse = DeleteTopicRuleDestinationResponse'
  { _dtrdrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteTopicRuleDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrdrsResponseStatus' - -- | The response status code.
deleteTopicRuleDestinationResponse ::
  -- | 'dtrdrsResponseStatus'
  Int ->
  DeleteTopicRuleDestinationResponse
deleteTopicRuleDestinationResponse pResponseStatus_ =
  DeleteTopicRuleDestinationResponse'
    { _dtrdrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dtrdrsResponseStatus :: Lens' DeleteTopicRuleDestinationResponse Int
dtrdrsResponseStatus = lens _dtrdrsResponseStatus (\s a -> s {_dtrdrsResponseStatus = a})

instance NFData DeleteTopicRuleDestinationResponse
