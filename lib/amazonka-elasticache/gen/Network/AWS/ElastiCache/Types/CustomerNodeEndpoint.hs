-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CustomerNodeEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CustomerNodeEndpoint
  ( CustomerNodeEndpoint (..),

    -- * Smart constructor
    mkCustomerNodeEndpoint,

    -- * Lenses
    cneAddress,
    cnePort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The endpoint from which data should be migrated.
--
-- /See:/ 'mkCustomerNodeEndpoint' smart constructor.
data CustomerNodeEndpoint = CustomerNodeEndpoint'
  { address ::
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

-- | Creates a value of 'CustomerNodeEndpoint' with the minimum fields required to make a request.
--
-- * 'address' - The address of the node endpoint
-- * 'port' - The port of the node endpoint
mkCustomerNodeEndpoint ::
  CustomerNodeEndpoint
mkCustomerNodeEndpoint =
  CustomerNodeEndpoint'
    { address = Lude.Nothing,
      port = Lude.Nothing
    }

-- | The address of the node endpoint
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cneAddress :: Lens.Lens' CustomerNodeEndpoint (Lude.Maybe Lude.Text)
cneAddress = Lens.lens (address :: CustomerNodeEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: CustomerNodeEndpoint)
{-# DEPRECATED cneAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | The port of the node endpoint
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnePort :: Lens.Lens' CustomerNodeEndpoint (Lude.Maybe Lude.Int)
cnePort = Lens.lens (port :: CustomerNodeEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: CustomerNodeEndpoint)
{-# DEPRECATED cnePort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.ToQuery CustomerNodeEndpoint where
  toQuery CustomerNodeEndpoint' {..} =
    Lude.mconcat ["Address" Lude.=: address, "Port" Lude.=: port]
