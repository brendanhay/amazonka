{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetHealth
  ( TargetHealth (..),

    -- * Smart constructor
    mkTargetHealth,

    -- * Lenses
    thState,
    thReason,
    thDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.TargetHealthReason
import Network.AWS.RDS.Types.TargetState

-- | Information about the connection health of an RDS Proxy target.
--
-- /See:/ 'mkTargetHealth' smart constructor.
data TargetHealth = TargetHealth'
  { state :: Lude.Maybe TargetState,
    reason :: Lude.Maybe TargetHealthReason,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetHealth' with the minimum fields required to make a request.
--
-- * 'description' - A description of the health of the RDS Proxy target. If the @State@ is @AVAILABLE@ , a description is not included.
-- * 'reason' - The reason for the current health @State@ of the RDS Proxy target.
-- * 'state' - The current state of the connection health lifecycle for the RDS Proxy target. The following is a typical lifecycle example for the states of an RDS Proxy target:
--
-- @registering@ > @unavailable@ > @available@ > @unavailable@ > @available@
mkTargetHealth ::
  TargetHealth
mkTargetHealth =
  TargetHealth'
    { state = Lude.Nothing,
      reason = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The current state of the connection health lifecycle for the RDS Proxy target. The following is a typical lifecycle example for the states of an RDS Proxy target:
--
-- @registering@ > @unavailable@ > @available@ > @unavailable@ > @available@
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thState :: Lens.Lens' TargetHealth (Lude.Maybe TargetState)
thState = Lens.lens (state :: TargetHealth -> Lude.Maybe TargetState) (\s a -> s {state = a} :: TargetHealth)
{-# DEPRECATED thState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason for the current health @State@ of the RDS Proxy target.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thReason :: Lens.Lens' TargetHealth (Lude.Maybe TargetHealthReason)
thReason = Lens.lens (reason :: TargetHealth -> Lude.Maybe TargetHealthReason) (\s a -> s {reason = a} :: TargetHealth)
{-# DEPRECATED thReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | A description of the health of the RDS Proxy target. If the @State@ is @AVAILABLE@ , a description is not included.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
thDescription :: Lens.Lens' TargetHealth (Lude.Maybe Lude.Text)
thDescription = Lens.lens (description :: TargetHealth -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: TargetHealth)
{-# DEPRECATED thDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML TargetHealth where
  parseXML x =
    TargetHealth'
      Lude.<$> (x Lude..@? "State")
      Lude.<*> (x Lude..@? "Reason")
      Lude.<*> (x Lude..@? "Description")
