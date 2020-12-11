-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplatesMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplatesMonitoring
  ( LaunchTemplatesMonitoring (..),

    -- * Smart constructor
    mkLaunchTemplatesMonitoring,

    -- * Lenses
    ltmEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the monitoring for the instance.
--
-- /See:/ 'mkLaunchTemplatesMonitoring' smart constructor.
newtype LaunchTemplatesMonitoring = LaunchTemplatesMonitoring'
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

-- | Creates a value of 'LaunchTemplatesMonitoring' with the minimum fields required to make a request.
--
-- * 'enabled' - Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
mkLaunchTemplatesMonitoring ::
  LaunchTemplatesMonitoring
mkLaunchTemplatesMonitoring =
  LaunchTemplatesMonitoring' {enabled = Lude.Nothing}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltmEnabled :: Lens.Lens' LaunchTemplatesMonitoring (Lude.Maybe Lude.Bool)
ltmEnabled = Lens.lens (enabled :: LaunchTemplatesMonitoring -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: LaunchTemplatesMonitoring)
{-# DEPRECATED ltmEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML LaunchTemplatesMonitoring where
  parseXML x =
    LaunchTemplatesMonitoring' Lude.<$> (x Lude..@? "enabled")
