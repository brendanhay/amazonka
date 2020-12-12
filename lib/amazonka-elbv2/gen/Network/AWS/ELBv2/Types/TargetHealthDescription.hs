{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealthDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetHealthDescription
  ( TargetHealthDescription (..),

    -- * Smart constructor
    mkTargetHealthDescription,

    -- * Lenses
    thdTargetHealth,
    thdHealthCheckPort,
    thdTarget,
  )
where

import Network.AWS.ELBv2.Types.TargetDescription
import Network.AWS.ELBv2.Types.TargetHealth
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the health of a target.
--
-- /See:/ 'mkTargetHealthDescription' smart constructor.
data TargetHealthDescription = TargetHealthDescription'
  { targetHealth ::
      Lude.Maybe TargetHealth,
    healthCheckPort :: Lude.Maybe Lude.Text,
    target :: Lude.Maybe TargetDescription
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetHealthDescription' with the minimum fields required to make a request.
--
-- * 'healthCheckPort' - The port to use to connect with the target.
-- * 'target' - The description of the target.
-- * 'targetHealth' - The health information for the target.
mkTargetHealthDescription ::
  TargetHealthDescription
mkTargetHealthDescription =
  TargetHealthDescription'
    { targetHealth = Lude.Nothing,
      healthCheckPort = Lude.Nothing,
      target = Lude.Nothing
    }

-- | The health information for the target.
--
-- /Note:/ Consider using 'targetHealth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thdTargetHealth :: Lens.Lens' TargetHealthDescription (Lude.Maybe TargetHealth)
thdTargetHealth = Lens.lens (targetHealth :: TargetHealthDescription -> Lude.Maybe TargetHealth) (\s a -> s {targetHealth = a} :: TargetHealthDescription)
{-# DEPRECATED thdTargetHealth "Use generic-lens or generic-optics with 'targetHealth' instead." #-}

-- | The port to use to connect with the target.
--
-- /Note:/ Consider using 'healthCheckPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thdHealthCheckPort :: Lens.Lens' TargetHealthDescription (Lude.Maybe Lude.Text)
thdHealthCheckPort = Lens.lens (healthCheckPort :: TargetHealthDescription -> Lude.Maybe Lude.Text) (\s a -> s {healthCheckPort = a} :: TargetHealthDescription)
{-# DEPRECATED thdHealthCheckPort "Use generic-lens or generic-optics with 'healthCheckPort' instead." #-}

-- | The description of the target.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thdTarget :: Lens.Lens' TargetHealthDescription (Lude.Maybe TargetDescription)
thdTarget = Lens.lens (target :: TargetHealthDescription -> Lude.Maybe TargetDescription) (\s a -> s {target = a} :: TargetHealthDescription)
{-# DEPRECATED thdTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromXML TargetHealthDescription where
  parseXML x =
    TargetHealthDescription'
      Lude.<$> (x Lude..@? "TargetHealth")
      Lude.<*> (x Lude..@? "HealthCheckPort")
      Lude.<*> (x Lude..@? "Target")
