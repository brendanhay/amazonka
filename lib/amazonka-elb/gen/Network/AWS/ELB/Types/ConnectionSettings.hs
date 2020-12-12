{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.ConnectionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.ConnectionSettings
  ( ConnectionSettings (..),

    -- * Smart constructor
    mkConnectionSettings,

    -- * Lenses
    csIdleTimeout,
  )
where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the @ConnectionSettings@ attribute.
--
-- /See:/ 'mkConnectionSettings' smart constructor.
newtype ConnectionSettings = ConnectionSettings'
  { idleTimeout ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionSettings' with the minimum fields required to make a request.
--
-- * 'idleTimeout' - The time, in seconds, that the connection is allowed to be idle (no data has been sent over the connection) before it is closed by the load balancer.
mkConnectionSettings ::
  -- | 'idleTimeout'
  Lude.Natural ->
  ConnectionSettings
mkConnectionSettings pIdleTimeout_ =
  ConnectionSettings' {idleTimeout = pIdleTimeout_}

-- | The time, in seconds, that the connection is allowed to be idle (no data has been sent over the connection) before it is closed by the load balancer.
--
-- /Note:/ Consider using 'idleTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csIdleTimeout :: Lens.Lens' ConnectionSettings Lude.Natural
csIdleTimeout = Lens.lens (idleTimeout :: ConnectionSettings -> Lude.Natural) (\s a -> s {idleTimeout = a} :: ConnectionSettings)
{-# DEPRECATED csIdleTimeout "Use generic-lens or generic-optics with 'idleTimeout' instead." #-}

instance Lude.FromXML ConnectionSettings where
  parseXML x = ConnectionSettings' Lude.<$> (x Lude..@ "IdleTimeout")

instance Lude.ToQuery ConnectionSettings where
  toQuery ConnectionSettings' {..} =
    Lude.mconcat ["IdleTimeout" Lude.=: idleTimeout]
