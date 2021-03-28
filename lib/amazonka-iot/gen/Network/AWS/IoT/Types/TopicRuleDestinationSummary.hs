{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestinationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TopicRuleDestinationSummary
  ( TopicRuleDestinationSummary (..)
  -- * Smart constructor
  , mkTopicRuleDestinationSummary
  -- * Lenses
  , trdsArn
  , trdsHttpUrlSummary
  , trdsStatus
  , trdsStatusReason
  ) where

import qualified Network.AWS.IoT.Types.Arn as Types
import qualified Network.AWS.IoT.Types.HttpUrlDestinationSummary as Types
import qualified Network.AWS.IoT.Types.TopicRuleDestinationStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the topic rule destination.
--
-- /See:/ 'mkTopicRuleDestinationSummary' smart constructor.
data TopicRuleDestinationSummary = TopicRuleDestinationSummary'
  { arn :: Core.Maybe Types.Arn
    -- ^ The topic rule destination ARN.
  , httpUrlSummary :: Core.Maybe Types.HttpUrlDestinationSummary
    -- ^ Information about the HTTP URL.
  , status :: Core.Maybe Types.TopicRuleDestinationStatus
    -- ^ The status of the topic rule destination. Valid values are:
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
  , statusReason :: Core.Maybe Core.Text
    -- ^ The reason the topic rule destination is in the current status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TopicRuleDestinationSummary' value with any optional fields omitted.
mkTopicRuleDestinationSummary
    :: TopicRuleDestinationSummary
mkTopicRuleDestinationSummary
  = TopicRuleDestinationSummary'{arn = Core.Nothing,
                                 httpUrlSummary = Core.Nothing, status = Core.Nothing,
                                 statusReason = Core.Nothing}

-- | The topic rule destination ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdsArn :: Lens.Lens' TopicRuleDestinationSummary (Core.Maybe Types.Arn)
trdsArn = Lens.field @"arn"
{-# INLINEABLE trdsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Information about the HTTP URL.
--
-- /Note:/ Consider using 'httpUrlSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdsHttpUrlSummary :: Lens.Lens' TopicRuleDestinationSummary (Core.Maybe Types.HttpUrlDestinationSummary)
trdsHttpUrlSummary = Lens.field @"httpUrlSummary"
{-# INLINEABLE trdsHttpUrlSummary #-}
{-# DEPRECATED httpUrlSummary "Use generic-lens or generic-optics with 'httpUrlSummary' instead"  #-}

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
trdsStatus :: Lens.Lens' TopicRuleDestinationSummary (Core.Maybe Types.TopicRuleDestinationStatus)
trdsStatus = Lens.field @"status"
{-# INLINEABLE trdsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The reason the topic rule destination is in the current status.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trdsStatusReason :: Lens.Lens' TopicRuleDestinationSummary (Core.Maybe Core.Text)
trdsStatusReason = Lens.field @"statusReason"
{-# INLINEABLE trdsStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

instance Core.FromJSON TopicRuleDestinationSummary where
        parseJSON
          = Core.withObject "TopicRuleDestinationSummary" Core.$
              \ x ->
                TopicRuleDestinationSummary' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "httpUrlSummary" Core.<*>
                    x Core..:? "status"
                    Core.<*> x Core..:? "statusReason"
