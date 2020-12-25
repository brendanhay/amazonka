{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ReputationOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ReputationOptions
  ( ReputationOptions (..),

    -- * Smart constructor
    mkReputationOptions,

    -- * Lenses
    roLastFreshStart,
    roReputationMetricsEnabled,
    roSendingEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the reputation settings for a configuration set.
--
-- /See:/ 'mkReputationOptions' smart constructor.
data ReputationOptions = ReputationOptions'
  { -- | The date and time at which the reputation metrics for the configuration set were last reset. Resetting these metrics is known as a /fresh start/ .
    --
    -- When you disable email sending for a configuration set using 'UpdateConfigurationSetSendingEnabled' and later re-enable it, the reputation metrics for the configuration set (but not for the entire Amazon SES account) are reset.
    -- If email sending for the configuration set has never been disabled and later re-enabled, the value of this attribute is @null@ .
    lastFreshStart :: Core.Maybe Core.UTCTime,
    -- | Describes whether or not Amazon SES publishes reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
    --
    -- If the value is @true@ , reputation metrics are published. If the value is @false@ , reputation metrics are not published. The default value is @false@ .
    reputationMetricsEnabled :: Core.Maybe Core.Bool,
    -- | Describes whether email sending is enabled or disabled for the configuration set. If the value is @true@ , then Amazon SES will send emails that use the configuration set. If the value is @false@ , Amazon SES will not send emails that use the configuration set. The default value is @true@ . You can change this setting using 'UpdateConfigurationSetSendingEnabled' .
    sendingEnabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReputationOptions' value with any optional fields omitted.
mkReputationOptions ::
  ReputationOptions
mkReputationOptions =
  ReputationOptions'
    { lastFreshStart = Core.Nothing,
      reputationMetricsEnabled = Core.Nothing,
      sendingEnabled = Core.Nothing
    }

-- | The date and time at which the reputation metrics for the configuration set were last reset. Resetting these metrics is known as a /fresh start/ .
--
-- When you disable email sending for a configuration set using 'UpdateConfigurationSetSendingEnabled' and later re-enable it, the reputation metrics for the configuration set (but not for the entire Amazon SES account) are reset.
-- If email sending for the configuration set has never been disabled and later re-enabled, the value of this attribute is @null@ .
--
-- /Note:/ Consider using 'lastFreshStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roLastFreshStart :: Lens.Lens' ReputationOptions (Core.Maybe Core.UTCTime)
roLastFreshStart = Lens.field @"lastFreshStart"
{-# DEPRECATED roLastFreshStart "Use generic-lens or generic-optics with 'lastFreshStart' instead." #-}

-- | Describes whether or not Amazon SES publishes reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
--
-- If the value is @true@ , reputation metrics are published. If the value is @false@ , reputation metrics are not published. The default value is @false@ .
--
-- /Note:/ Consider using 'reputationMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roReputationMetricsEnabled :: Lens.Lens' ReputationOptions (Core.Maybe Core.Bool)
roReputationMetricsEnabled = Lens.field @"reputationMetricsEnabled"
{-# DEPRECATED roReputationMetricsEnabled "Use generic-lens or generic-optics with 'reputationMetricsEnabled' instead." #-}

-- | Describes whether email sending is enabled or disabled for the configuration set. If the value is @true@ , then Amazon SES will send emails that use the configuration set. If the value is @false@ , Amazon SES will not send emails that use the configuration set. The default value is @true@ . You can change this setting using 'UpdateConfigurationSetSendingEnabled' .
--
-- /Note:/ Consider using 'sendingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roSendingEnabled :: Lens.Lens' ReputationOptions (Core.Maybe Core.Bool)
roSendingEnabled = Lens.field @"sendingEnabled"
{-# DEPRECATED roSendingEnabled "Use generic-lens or generic-optics with 'sendingEnabled' instead." #-}

instance Core.FromXML ReputationOptions where
  parseXML x =
    ReputationOptions'
      Core.<$> (x Core..@? "LastFreshStart")
      Core.<*> (x Core..@? "ReputationMetricsEnabled")
      Core.<*> (x Core..@? "SendingEnabled")
