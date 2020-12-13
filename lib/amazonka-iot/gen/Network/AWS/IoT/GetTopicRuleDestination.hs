{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    GetTopicRuleDestination (..),
    mkGetTopicRuleDestination,

    -- ** Request lenses
    gtrdArn,

    -- * Destructuring the response
    GetTopicRuleDestinationResponse (..),
    mkGetTopicRuleDestinationResponse,

    -- ** Response lenses
    gtrdrsTopicRuleDestination,
    gtrdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTopicRuleDestination' smart constructor.
newtype GetTopicRuleDestination = GetTopicRuleDestination'
  { -- | The ARN of the topic rule destination.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTopicRuleDestination' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the topic rule destination.
mkGetTopicRuleDestination ::
  -- | 'arn'
  Lude.Text ->
  GetTopicRuleDestination
mkGetTopicRuleDestination pArn_ =
  GetTopicRuleDestination' {arn = pArn_}

-- | The ARN of the topic rule destination.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrdArn :: Lens.Lens' GetTopicRuleDestination Lude.Text
gtrdArn = Lens.lens (arn :: GetTopicRuleDestination -> Lude.Text) (\s a -> s {arn = a} :: GetTopicRuleDestination)
{-# DEPRECATED gtrdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetTopicRuleDestination where
  type Rs GetTopicRuleDestination = GetTopicRuleDestinationResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTopicRuleDestinationResponse'
            Lude.<$> (x Lude..?> "topicRuleDestination")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTopicRuleDestination where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetTopicRuleDestination where
  toPath GetTopicRuleDestination' {..} =
    Lude.mconcat ["/destinations/", Lude.toBS arn]

instance Lude.ToQuery GetTopicRuleDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTopicRuleDestinationResponse' smart constructor.
data GetTopicRuleDestinationResponse = GetTopicRuleDestinationResponse'
  { -- | The topic rule destination.
    topicRuleDestination :: Lude.Maybe TopicRuleDestination,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTopicRuleDestinationResponse' with the minimum fields required to make a request.
--
-- * 'topicRuleDestination' - The topic rule destination.
-- * 'responseStatus' - The response status code.
mkGetTopicRuleDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTopicRuleDestinationResponse
mkGetTopicRuleDestinationResponse pResponseStatus_ =
  GetTopicRuleDestinationResponse'
    { topicRuleDestination =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The topic rule destination.
--
-- /Note:/ Consider using 'topicRuleDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrdrsTopicRuleDestination :: Lens.Lens' GetTopicRuleDestinationResponse (Lude.Maybe TopicRuleDestination)
gtrdrsTopicRuleDestination = Lens.lens (topicRuleDestination :: GetTopicRuleDestinationResponse -> Lude.Maybe TopicRuleDestination) (\s a -> s {topicRuleDestination = a} :: GetTopicRuleDestinationResponse)
{-# DEPRECATED gtrdrsTopicRuleDestination "Use generic-lens or generic-optics with 'topicRuleDestination' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrdrsResponseStatus :: Lens.Lens' GetTopicRuleDestinationResponse Lude.Int
gtrdrsResponseStatus = Lens.lens (responseStatus :: GetTopicRuleDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTopicRuleDestinationResponse)
{-# DEPRECATED gtrdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
