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
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the reputation settings for a configuration set.
--
-- /See:/ 'mkReputationOptions' smart constructor.
data ReputationOptions = ReputationOptions'
  { lastFreshStart ::
      Lude.Maybe Lude.ISO8601,
    reputationMetricsEnabled :: Lude.Maybe Lude.Bool,
    sendingEnabled :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReputationOptions' with the minimum fields required to make a request.
--
-- * 'lastFreshStart' - The date and time at which the reputation metrics for the configuration set were last reset. Resetting these metrics is known as a /fresh start/ .
--
-- When you disable email sending for a configuration set using 'UpdateConfigurationSetSendingEnabled' and later re-enable it, the reputation metrics for the configuration set (but not for the entire Amazon SES account) are reset.
-- If email sending for the configuration set has never been disabled and later re-enabled, the value of this attribute is @null@ .
-- * 'reputationMetricsEnabled' - Describes whether or not Amazon SES publishes reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
--
-- If the value is @true@ , reputation metrics are published. If the value is @false@ , reputation metrics are not published. The default value is @false@ .
-- * 'sendingEnabled' - Describes whether email sending is enabled or disabled for the configuration set. If the value is @true@ , then Amazon SES will send emails that use the configuration set. If the value is @false@ , Amazon SES will not send emails that use the configuration set. The default value is @true@ . You can change this setting using 'UpdateConfigurationSetSendingEnabled' .
mkReputationOptions ::
  ReputationOptions
mkReputationOptions =
  ReputationOptions'
    { lastFreshStart = Lude.Nothing,
      reputationMetricsEnabled = Lude.Nothing,
      sendingEnabled = Lude.Nothing
    }

-- | The date and time at which the reputation metrics for the configuration set were last reset. Resetting these metrics is known as a /fresh start/ .
--
-- When you disable email sending for a configuration set using 'UpdateConfigurationSetSendingEnabled' and later re-enable it, the reputation metrics for the configuration set (but not for the entire Amazon SES account) are reset.
-- If email sending for the configuration set has never been disabled and later re-enabled, the value of this attribute is @null@ .
--
-- /Note:/ Consider using 'lastFreshStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roLastFreshStart :: Lens.Lens' ReputationOptions (Lude.Maybe Lude.ISO8601)
roLastFreshStart = Lens.lens (lastFreshStart :: ReputationOptions -> Lude.Maybe Lude.ISO8601) (\s a -> s {lastFreshStart = a} :: ReputationOptions)
{-# DEPRECATED roLastFreshStart "Use generic-lens or generic-optics with 'lastFreshStart' instead." #-}

-- | Describes whether or not Amazon SES publishes reputation metrics for the configuration set, such as bounce and complaint rates, to Amazon CloudWatch.
--
-- If the value is @true@ , reputation metrics are published. If the value is @false@ , reputation metrics are not published. The default value is @false@ .
--
-- /Note:/ Consider using 'reputationMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roReputationMetricsEnabled :: Lens.Lens' ReputationOptions (Lude.Maybe Lude.Bool)
roReputationMetricsEnabled = Lens.lens (reputationMetricsEnabled :: ReputationOptions -> Lude.Maybe Lude.Bool) (\s a -> s {reputationMetricsEnabled = a} :: ReputationOptions)
{-# DEPRECATED roReputationMetricsEnabled "Use generic-lens or generic-optics with 'reputationMetricsEnabled' instead." #-}

-- | Describes whether email sending is enabled or disabled for the configuration set. If the value is @true@ , then Amazon SES will send emails that use the configuration set. If the value is @false@ , Amazon SES will not send emails that use the configuration set. The default value is @true@ . You can change this setting using 'UpdateConfigurationSetSendingEnabled' .
--
-- /Note:/ Consider using 'sendingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
roSendingEnabled :: Lens.Lens' ReputationOptions (Lude.Maybe Lude.Bool)
roSendingEnabled = Lens.lens (sendingEnabled :: ReputationOptions -> Lude.Maybe Lude.Bool) (\s a -> s {sendingEnabled = a} :: ReputationOptions)
{-# DEPRECATED roSendingEnabled "Use generic-lens or generic-optics with 'sendingEnabled' instead." #-}

instance Lude.FromXML ReputationOptions where
  parseXML x =
    ReputationOptions'
      Lude.<$> (x Lude..@? "LastFreshStart")
      Lude.<*> (x Lude..@? "ReputationMetricsEnabled")
      Lude.<*> (x Lude..@? "SendingEnabled")
