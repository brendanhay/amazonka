{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestination
  ( TopicRuleDestination (..),

    -- * Smart constructor
    mkTopicRuleDestination,

    -- * Lenses
    trdArn,
    trdHttpUrlProperties,
    trdStatus,
    trdStatusReason,
  )
where

import qualified Network.AWS.IoT.Types.Arn as Types
import qualified Network.AWS.IoT.Types.HttpUrlDestinationProperties as Types
import qualified Network.AWS.IoT.Types.String as Types
import qualified Network.AWS.IoT.Types.TopicRuleDestinationStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A topic rule destination.
--
-- /See:/ 'mkTopicRuleDestination' smart constructor.
data TopicRuleDestination = TopicRuleDestination'
  { -- | The topic rule destination URL.
    arn :: Core.Maybe Types.Arn,
    -- | Properties of the HTTP URL.
    httpUrlProperties :: Core.Maybe Types.HttpUrlDestinationProperties,
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
    status :: Core.Maybe Types.TopicRuleDestinationStatus,
    -- | Additional details or reason why the topic rule destination is in the current status.
    statusReason :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TopicRuleDestination' value with any optional fields omitted.
mkTopicRuleDestination ::
  TopicRuleDestination
mkTopicRuleDestination =
  TopicRuleDestination'
    { arn = Core.Nothing,
      httpUrlProperties = Core.Nothing,
      status = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | The topic rule destination URL.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdArn :: Lens.Lens' TopicRuleDestination (Core.Maybe Types.Arn)
trdArn = Lens.field @"arn"
{-# DEPRECATED trdArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Properties of the HTTP URL.
--
-- /Note:/ Consider using 'httpUrlProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdHttpUrlProperties :: Lens.Lens' TopicRuleDestination (Core.Maybe Types.HttpUrlDestinationProperties)
trdHttpUrlProperties = Lens.field @"httpUrlProperties"
{-# DEPRECATED trdHttpUrlProperties "Use generic-lens or generic-optics with 'httpUrlProperties' instead." #-}

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
trdStatus :: Lens.Lens' TopicRuleDestination (Core.Maybe Types.TopicRuleDestinationStatus)
trdStatus = Lens.field @"status"
{-# DEPRECATED trdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Additional details or reason why the topic rule destination is in the current status.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdStatusReason :: Lens.Lens' TopicRuleDestination (Core.Maybe Types.String)
trdStatusReason = Lens.field @"statusReason"
{-# DEPRECATED trdStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

instance Core.FromJSON TopicRuleDestination where
  parseJSON =
    Core.withObject "TopicRuleDestination" Core.$
      \x ->
        TopicRuleDestination'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "httpUrlProperties")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "statusReason")
