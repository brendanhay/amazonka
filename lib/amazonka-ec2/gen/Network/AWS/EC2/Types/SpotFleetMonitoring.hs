{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotFleetMonitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotFleetMonitoring
  ( SpotFleetMonitoring (..),

    -- * Smart constructor
    mkSpotFleetMonitoring,

    -- * Lenses
    sfmEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes whether monitoring is enabled.
--
-- /See:/ 'mkSpotFleetMonitoring' smart constructor.
newtype SpotFleetMonitoring = SpotFleetMonitoring'
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

-- | Creates a value of 'SpotFleetMonitoring' with the minimum fields required to make a request.
--
-- * 'enabled' - Enables monitoring for the instance.
--
-- Default: @false@
mkSpotFleetMonitoring ::
  SpotFleetMonitoring
mkSpotFleetMonitoring =
  SpotFleetMonitoring' {enabled = Lude.Nothing}

-- | Enables monitoring for the instance.
--
-- Default: @false@
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfmEnabled :: Lens.Lens' SpotFleetMonitoring (Lude.Maybe Lude.Bool)
sfmEnabled = Lens.lens (enabled :: SpotFleetMonitoring -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: SpotFleetMonitoring)
{-# DEPRECATED sfmEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML SpotFleetMonitoring where
  parseXML x = SpotFleetMonitoring' Lude.<$> (x Lude..@? "enabled")

instance Lude.ToQuery SpotFleetMonitoring where
  toQuery SpotFleetMonitoring' {..} =
    Lude.mconcat ["Enabled" Lude.=: enabled]
