{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eAddress,
    ePort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes a connection endpoint.
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { address :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- * 'address' - The DNS address of the Cluster.
-- * 'port' - The port that the database engine is listening on.
mkEndpoint ::
  Endpoint
mkEndpoint = Endpoint' {address = Lude.Nothing, port = Lude.Nothing}

-- | The DNS address of the Cluster.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAddress :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eAddress = Lens.lens (address :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: Endpoint)
{-# DEPRECATED eAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The port that the database engine is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePort :: Lens.Lens' Endpoint (Lude.Maybe Lude.Int)
ePort = Lens.lens (port :: Endpoint -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: Endpoint)
{-# DEPRECATED ePort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Lude.<$> (x Lude..@? "Address") Lude.<*> (x Lude..@? "Port")
