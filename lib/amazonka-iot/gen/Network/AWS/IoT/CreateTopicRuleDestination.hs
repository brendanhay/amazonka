{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateTopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a topic rule destination. The destination must be confirmed prior to use.
module Network.AWS.IoT.CreateTopicRuleDestination
  ( -- * Creating a request
    CreateTopicRuleDestination (..),
    mkCreateTopicRuleDestination,

    -- ** Request lenses
    ctrdDestinationConfiguration,

    -- * Destructuring the response
    CreateTopicRuleDestinationResponse (..),
    mkCreateTopicRuleDestinationResponse,

    -- ** Response lenses
    crsTopicRuleDestination,
    crsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateTopicRuleDestination' smart constructor.
newtype CreateTopicRuleDestination = CreateTopicRuleDestination'
  { -- | The topic rule destination configuration.
    destinationConfiguration :: TopicRuleDestinationConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTopicRuleDestination' with the minimum fields required to make a request.
--
-- * 'destinationConfiguration' - The topic rule destination configuration.
mkCreateTopicRuleDestination ::
  -- | 'destinationConfiguration'
  TopicRuleDestinationConfiguration ->
  CreateTopicRuleDestination
mkCreateTopicRuleDestination pDestinationConfiguration_ =
  CreateTopicRuleDestination'
    { destinationConfiguration =
        pDestinationConfiguration_
    }

-- | The topic rule destination configuration.
--
-- /Note:/ Consider using 'destinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctrdDestinationConfiguration :: Lens.Lens' CreateTopicRuleDestination TopicRuleDestinationConfiguration
ctrdDestinationConfiguration = Lens.lens (destinationConfiguration :: CreateTopicRuleDestination -> TopicRuleDestinationConfiguration) (\s a -> s {destinationConfiguration = a} :: CreateTopicRuleDestination)
{-# DEPRECATED ctrdDestinationConfiguration "Use generic-lens or generic-optics with 'destinationConfiguration' instead." #-}

instance Lude.AWSRequest CreateTopicRuleDestination where
  type
    Rs CreateTopicRuleDestination =
      CreateTopicRuleDestinationResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTopicRuleDestinationResponse'
            Lude.<$> (x Lude..?> "topicRuleDestination")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTopicRuleDestination where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateTopicRuleDestination where
  toJSON CreateTopicRuleDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("destinationConfiguration" Lude..= destinationConfiguration)
          ]
      )

instance Lude.ToPath CreateTopicRuleDestination where
  toPath = Lude.const "/destinations"

instance Lude.ToQuery CreateTopicRuleDestination where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTopicRuleDestinationResponse' smart constructor.
data CreateTopicRuleDestinationResponse = CreateTopicRuleDestinationResponse'
  { -- | The topic rule destination.
    topicRuleDestination :: Lude.Maybe TopicRuleDestination,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTopicRuleDestinationResponse' with the minimum fields required to make a request.
--
-- * 'topicRuleDestination' - The topic rule destination.
-- * 'responseStatus' - The response status code.
mkCreateTopicRuleDestinationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTopicRuleDestinationResponse
mkCreateTopicRuleDestinationResponse pResponseStatus_ =
  CreateTopicRuleDestinationResponse'
    { topicRuleDestination =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The topic rule destination.
--
-- /Note:/ Consider using 'topicRuleDestination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsTopicRuleDestination :: Lens.Lens' CreateTopicRuleDestinationResponse (Lude.Maybe TopicRuleDestination)
crsTopicRuleDestination = Lens.lens (topicRuleDestination :: CreateTopicRuleDestinationResponse -> Lude.Maybe TopicRuleDestination) (\s a -> s {topicRuleDestination = a} :: CreateTopicRuleDestinationResponse)
{-# DEPRECATED crsTopicRuleDestination "Use generic-lens or generic-optics with 'topicRuleDestination' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsResponseStatus :: Lens.Lens' CreateTopicRuleDestinationResponse Lude.Int
crsResponseStatus = Lens.lens (responseStatus :: CreateTopicRuleDestinationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTopicRuleDestinationResponse)
{-# DEPRECATED crsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
