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
-- Module      : Network.AWS.IoT.UpdateTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a topic rule destination. You use this to change the status, endpoint URL, or confirmation URL of the destination.
module Network.AWS.IoT.UpdateTopicRuleDestination
  ( -- * Creating a Request
    updateTopicRuleDestination,
    UpdateTopicRuleDestination,

    -- * Request Lenses
    utrdArn,
    utrdStatus,

    -- * Destructuring the Response
    updateTopicRuleDestinationResponse,
    UpdateTopicRuleDestinationResponse,

    -- * Response Lenses
    utrdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateTopicRuleDestination' smart constructor.
data UpdateTopicRuleDestination = UpdateTopicRuleDestination'
  { _utrdArn ::
      !Text,
    _utrdStatus ::
      !TopicRuleDestinationStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTopicRuleDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrdArn' - The ARN of the topic rule destination.
--
-- * 'utrdStatus' - The status of the topic rule destination. Valid values are:     * IN_PROGRESS    * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.     * ENABLED    * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .     * DISABLED    * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .     * ERROR    * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
updateTopicRuleDestination ::
  -- | 'utrdArn'
  Text ->
  -- | 'utrdStatus'
  TopicRuleDestinationStatus ->
  UpdateTopicRuleDestination
updateTopicRuleDestination pArn_ pStatus_ =
  UpdateTopicRuleDestination'
    { _utrdArn = pArn_,
      _utrdStatus = pStatus_
    }

-- | The ARN of the topic rule destination.
utrdArn :: Lens' UpdateTopicRuleDestination Text
utrdArn = lens _utrdArn (\s a -> s {_utrdArn = a})

-- | The status of the topic rule destination. Valid values are:     * IN_PROGRESS    * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.     * ENABLED    * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .     * DISABLED    * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .     * ERROR    * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
utrdStatus :: Lens' UpdateTopicRuleDestination TopicRuleDestinationStatus
utrdStatus = lens _utrdStatus (\s a -> s {_utrdStatus = a})

instance AWSRequest UpdateTopicRuleDestination where
  type
    Rs UpdateTopicRuleDestination =
      UpdateTopicRuleDestinationResponse
  request = patchJSON ioT
  response =
    receiveEmpty
      ( \s h x ->
          UpdateTopicRuleDestinationResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateTopicRuleDestination

instance NFData UpdateTopicRuleDestination

instance ToHeaders UpdateTopicRuleDestination where
  toHeaders = const mempty

instance ToJSON UpdateTopicRuleDestination where
  toJSON UpdateTopicRuleDestination' {..} =
    object
      ( catMaybes
          [Just ("arn" .= _utrdArn), Just ("status" .= _utrdStatus)]
      )

instance ToPath UpdateTopicRuleDestination where
  toPath = const "/destinations"

instance ToQuery UpdateTopicRuleDestination where
  toQuery = const mempty

-- | /See:/ 'updateTopicRuleDestinationResponse' smart constructor.
newtype UpdateTopicRuleDestinationResponse = UpdateTopicRuleDestinationResponse'
  { _utrdrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTopicRuleDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utrdrsResponseStatus' - -- | The response status code.
updateTopicRuleDestinationResponse ::
  -- | 'utrdrsResponseStatus'
  Int ->
  UpdateTopicRuleDestinationResponse
updateTopicRuleDestinationResponse pResponseStatus_ =
  UpdateTopicRuleDestinationResponse'
    { _utrdrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
utrdrsResponseStatus :: Lens' UpdateTopicRuleDestinationResponse Int
utrdrsResponseStatus = lens _utrdrsResponseStatus (\s a -> s {_utrdrsResponseStatus = a})

instance NFData UpdateTopicRuleDestinationResponse
