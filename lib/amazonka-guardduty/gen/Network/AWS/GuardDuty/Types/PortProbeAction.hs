-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PortProbeAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PortProbeAction
  ( PortProbeAction (..),

    -- * Smart constructor
    mkPortProbeAction,

    -- * Lenses
    ppaPortProbeDetails,
    ppaBlocked,
  )
where

import Network.AWS.GuardDuty.Types.PortProbeDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the PORT_PROBE action described in the finding.
--
-- /See:/ 'mkPortProbeAction' smart constructor.
data PortProbeAction = PortProbeAction'
  { portProbeDetails ::
      Lude.Maybe [PortProbeDetail],
    blocked :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PortProbeAction' with the minimum fields required to make a request.
--
-- * 'blocked' - Indicates whether EC2 blocked the port probe to the instance, such as with an ACL.
-- * 'portProbeDetails' - A list of objects related to port probe details.
mkPortProbeAction ::
  PortProbeAction
mkPortProbeAction =
  PortProbeAction'
    { portProbeDetails = Lude.Nothing,
      blocked = Lude.Nothing
    }

-- | A list of objects related to port probe details.
--
-- /Note:/ Consider using 'portProbeDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaPortProbeDetails :: Lens.Lens' PortProbeAction (Lude.Maybe [PortProbeDetail])
ppaPortProbeDetails = Lens.lens (portProbeDetails :: PortProbeAction -> Lude.Maybe [PortProbeDetail]) (\s a -> s {portProbeDetails = a} :: PortProbeAction)
{-# DEPRECATED ppaPortProbeDetails "Use generic-lens or generic-optics with 'portProbeDetails' instead." #-}

-- | Indicates whether EC2 blocked the port probe to the instance, such as with an ACL.
--
-- /Note:/ Consider using 'blocked' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppaBlocked :: Lens.Lens' PortProbeAction (Lude.Maybe Lude.Bool)
ppaBlocked = Lens.lens (blocked :: PortProbeAction -> Lude.Maybe Lude.Bool) (\s a -> s {blocked = a} :: PortProbeAction)
{-# DEPRECATED ppaBlocked "Use generic-lens or generic-optics with 'blocked' instead." #-}

instance Lude.FromJSON PortProbeAction where
  parseJSON =
    Lude.withObject
      "PortProbeAction"
      ( \x ->
          PortProbeAction'
            Lude.<$> (x Lude..:? "portProbeDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "blocked")
      )
