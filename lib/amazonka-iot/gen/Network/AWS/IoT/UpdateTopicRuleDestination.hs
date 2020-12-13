{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    UpdateTopicRuleDestination (..),
    mkUpdateTopicRuleDestination,

    -- ** Request lenses
    utrdStatus,
    utrdArn,

    -- * Destructuring the response
    UpdateTopicRuleDestinationResponse (..),
    mkUpdateTopicRuleDestinationResponse,

    -- ** Response lenses
    utrdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTopicRuleDestination' smart constructor.
data UpdateTopicRuleDestination = UpdateTopicRuleDestination'
  { -- | The status of the topic rule destination. Valid values are:
    --
    --
    --     * IN_PROGRESS
    --
    --     * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
    --
    --
    --     * ENABLED
    --
    --     * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .
    --
    --
    --     * DISABLED
    --
    --     * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .
    --
    --
    --     * ERROR
    --
    --     * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
    status :: TopicRuleDestinationStatus,
    -- | The ARN of the topic rule destination.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTopicRuleDestination' with the minimum fields required to make a request.
--
-- * 'status' - The status of the topic rule destination. Valid values are:
--
--
--     * IN_PROGRESS
--
--     * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
--
--     * ENABLED
--
--     * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .
--
--
--     * DISABLED
--
--     * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .
--
--
--     * ERROR
--
--     * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
--
-- * 'arn' - The ARN of the topic rule destination.
mkUpdateTopicRuleDestination ::
  -- | 'status'
  TopicRuleDestinationStatus ->
  -- | 'arn'
  Lude.Text ->
  UpdateTopicRuleDestination
mkUpdateTopicRuleDestination pStatus_ pArn_ =
  UpdateTopicRuleDestination' {status = pStatus_, arn = pArn_}

-- | The status of the topic rule destination. Valid values are:
--
--
--     * IN_PROGRESS
--
--     * A topic rule destination was created but has not been confirmed. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
--
--     * ENABLED
--
--     * Confirmation was completed, and traffic to this destination is allowed. You can set @status@ to @DISABLED@ by calling @UpdateTopicRuleDestination@ .
--
--
--     * DISABLED
--
--     * Confirmation was completed, and traffic to this destination is not allowed. You can set @status@ to @ENABLED@ by calling @UpdateTopicRuleDestination@ .
--
--
--     * ERROR
--
--     * Confirmation could not be completed, for example if the confirmation timed out. You can call @GetTopicRuleDestination@ for details about the error. You can set @status@ to @IN_PROGRESS@ by calling @UpdateTopicRuleDestination@ . Calling @UpdateTopicRuleDestination@ causes a new confirmation challenge to be sent to your confirmation endpoint.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrdStatus :: Lens.Lens' UpdateTopicRuleDestination TopicRuleDestinationStatus
utrdStatus = Lens.lens (status :: UpdateTopicRuleDestination -> TopicRuleDestinationStatus) (\s a -> s {status = a} :: UpdateTopicRuleDestination)
{-# DEPRECATED utrdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the topic rule destination.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrdArn :: Lens.Lens' UpdateTopicRuleDestination Lude.Text
utrdArn = Lens.lens (arn :: UpdateTopicRuleDestination -> Lude.Text) (\s a -> s {arn = a} :: UpdateTopicRuleDestination)
{-# DEPRECATED utrdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest UpdateTopicRuleDestination where
  type
    Rs UpdateTopicRuleDestination =
      UpdateTopicRuleDestinationResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateTopicRuleDestinationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTopicRuleDestination where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateTopicRuleDestination where
  toJSON UpdateTopicRuleDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("status" Lude..= status),
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath UpdateTopicRuleDestination where
  toPath = Lude.const "/destinations"

instance Lude.ToQuery UpdateTopicRuleDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTopicRuleDestinationResponse' smart constructor.
newtype UpdateTopicRuleDestinationResponse = UpdateTopicRuleDestinationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTopicRuleDestinationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateTopicRuleDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTopicRuleDestinationResponse
mkUpdateTopicRuleDestinationResponse pResponseStatus_ =
  UpdateTopicRuleDestinationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utrdrsResponseStatus :: Lens.Lens' UpdateTopicRuleDestinationResponse Lude.Int
utrdrsResponseStatus = Lens.lens (responseStatus :: UpdateTopicRuleDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTopicRuleDestinationResponse)
{-# DEPRECATED utrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
