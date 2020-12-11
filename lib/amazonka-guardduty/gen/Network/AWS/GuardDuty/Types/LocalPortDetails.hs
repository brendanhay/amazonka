-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.LocalPortDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.LocalPortDetails
  ( LocalPortDetails (..),

    -- * Smart constructor
    mkLocalPortDetails,

    -- * Lenses
    lpdPortName,
    lpdPort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the port for the local connection.
--
-- /See:/ 'mkLocalPortDetails' smart constructor.
data LocalPortDetails = LocalPortDetails'
  { portName ::
      Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LocalPortDetails' with the minimum fields required to make a request.
--
-- * 'port' - The port number of the local connection.
-- * 'portName' - The port name of the local connection.
mkLocalPortDetails ::
  LocalPortDetails
mkLocalPortDetails =
  LocalPortDetails' {portName = Lude.Nothing, port = Lude.Nothing}

-- | The port name of the local connection.
--
-- /Note:/ Consider using 'portName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdPortName :: Lens.Lens' LocalPortDetails (Lude.Maybe Lude.Text)
lpdPortName = Lens.lens (portName :: LocalPortDetails -> Lude.Maybe Lude.Text) (\s a -> s {portName = a} :: LocalPortDetails)
{-# DEPRECATED lpdPortName "Use generic-lens or generic-optics with 'portName' instead." #-}

-- | The port number of the local connection.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpdPort :: Lens.Lens' LocalPortDetails (Lude.Maybe Lude.Int)
lpdPort = Lens.lens (port :: LocalPortDetails -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: LocalPortDetails)
{-# DEPRECATED lpdPort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON LocalPortDetails where
  parseJSON =
    Lude.withObject
      "LocalPortDetails"
      ( \x ->
          LocalPortDetails'
            Lude.<$> (x Lude..:? "portName") Lude.<*> (x Lude..:? "port")
      )
