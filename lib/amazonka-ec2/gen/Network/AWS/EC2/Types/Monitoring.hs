{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Monitoring
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Monitoring
  ( Monitoring (..),

    -- * Smart constructor
    mkMonitoring,

    -- * Lenses
    mState,
  )
where

import Network.AWS.EC2.Types.MonitoringState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the monitoring of an instance.
--
-- /See:/ 'mkMonitoring' smart constructor.
newtype Monitoring = Monitoring'
  { -- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
    state :: Lude.Maybe MonitoringState
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Monitoring' with the minimum fields required to make a request.
--
-- * 'state' - Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
mkMonitoring ::
  Monitoring
mkMonitoring = Monitoring' {state = Lude.Nothing}

-- | Indicates whether detailed monitoring is enabled. Otherwise, basic monitoring is enabled.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mState :: Lens.Lens' Monitoring (Lude.Maybe MonitoringState)
mState = Lens.lens (state :: Monitoring -> Lude.Maybe MonitoringState) (\s a -> s {state = a} :: Monitoring)
{-# DEPRECATED mState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Lude.FromXML Monitoring where
  parseXML x = Monitoring' Lude.<$> (x Lude..@? "state")
