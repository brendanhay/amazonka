{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
  ( -- * Creating a request
    DeleteTopicRuleDestination (..),
    mkDeleteTopicRuleDestination,

    -- ** Request lenses
    dtrdArn,

    -- * Destructuring the response
    DeleteTopicRuleDestinationResponse (..),
    mkDeleteTopicRuleDestinationResponse,

    -- ** Response lenses
    dtrdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTopicRuleDestination' smart constructor.
newtype DeleteTopicRuleDestination = DeleteTopicRuleDestination'
  { arn ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTopicRuleDestination' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the topic rule destination to delete.
mkDeleteTopicRuleDestination ::
  -- | 'arn'
  Lude.Text ->
  DeleteTopicRuleDestination
mkDeleteTopicRuleDestination pArn_ =
  DeleteTopicRuleDestination' {arn = pArn_}

-- | The ARN of the topic rule destination to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrdArn :: Lens.Lens' DeleteTopicRuleDestination Lude.Text
dtrdArn = Lens.lens (arn :: DeleteTopicRuleDestination -> Lude.Text) (\s a -> s {arn = a} :: DeleteTopicRuleDestination)
{-# DEPRECATED dtrdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteTopicRuleDestination where
  type
    Rs DeleteTopicRuleDestination =
      DeleteTopicRuleDestinationResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteTopicRuleDestinationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTopicRuleDestination where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTopicRuleDestination where
  toPath DeleteTopicRuleDestination' {..} =
    Lude.mconcat ["/destinations/", Lude.toBS arn]

instance Lude.ToQuery DeleteTopicRuleDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteTopicRuleDestinationResponse' smart constructor.
newtype DeleteTopicRuleDestinationResponse = DeleteTopicRuleDestinationResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTopicRuleDestinationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTopicRuleDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTopicRuleDestinationResponse
mkDeleteTopicRuleDestinationResponse pResponseStatus_ =
  DeleteTopicRuleDestinationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrdrsResponseStatus :: Lens.Lens' DeleteTopicRuleDestinationResponse Lude.Int
dtrdrsResponseStatus = Lens.lens (responseStatus :: DeleteTopicRuleDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTopicRuleDestinationResponse)
{-# DEPRECATED dtrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
