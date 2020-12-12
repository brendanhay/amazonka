{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatesMonitoringRequest
  ( LaunchTemplatesMonitoringRequest (..),

    -- * Smart constructor
    mkLaunchTemplatesMonitoringRequest,

    -- * Lenses
    ltmrEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the monitoring for the instance.
--
-- /See:/ 'mkLaunchTemplatesMonitoringRequest' smart constructor.
newtype LaunchTemplatesMonitoringRequest = LaunchTemplatesMonitoringRequest'
  { enabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplatesMonitoringRequest' with the minimum fields required to make a request.
--
-- * 'enabled' - Specify @true@ to enable detailed monitoring. Otherwise, basic monitoring is enabled.
mkLaunchTemplatesMonitoringRequest ::
  LaunchTemplatesMonitoringRequest
mkLaunchTemplatesMonitoringRequest =
  LaunchTemplatesMonitoringRequest' {enabled = Lude.Nothing}

-- | Specify @true@ to enable detailed monitoring. Otherwise, basic monitoring is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmrEnabled :: Lens.Lens' LaunchTemplatesMonitoringRequest (Lude.Maybe Lude.Bool)
ltmrEnabled = Lens.lens (enabled :: LaunchTemplatesMonitoringRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: LaunchTemplatesMonitoringRequest)
{-# DEPRECATED ltmrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.ToQuery LaunchTemplatesMonitoringRequest where
  toQuery LaunchTemplatesMonitoringRequest' {..} =
    Lude.mconcat ["Enabled" Lude.=: enabled]
