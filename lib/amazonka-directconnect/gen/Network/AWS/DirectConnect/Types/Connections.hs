{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.Connections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Connections
  ( Connections (..),

    -- * Smart constructor
    mkConnections,

    -- * Lenses
    cConnections,
  )
where

import Network.AWS.DirectConnect.Types.Connection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkConnections' smart constructor.
newtype Connections = Connections'
  { connections ::
      Lude.Maybe [Connection]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Connections' with the minimum fields required to make a request.
--
-- * 'connections' - The connections.
mkConnections ::
  Connections
mkConnections = Connections' {connections = Lude.Nothing}

-- | The connections.
--
-- /Note:/ Consider using 'connections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConnections :: Lens.Lens' Connections (Lude.Maybe [Connection])
cConnections = Lens.lens (connections :: Connections -> Lude.Maybe [Connection]) (\s a -> s {connections = a} :: Connections)
{-# DEPRECATED cConnections "Use generic-lens or generic-optics with 'connections' instead." #-}

instance Lude.FromJSON Connections where
  parseJSON =
    Lude.withObject
      "Connections"
      ( \x ->
          Connections'
            Lude.<$> (x Lude..:? "connections" Lude..!= Lude.mempty)
      )
