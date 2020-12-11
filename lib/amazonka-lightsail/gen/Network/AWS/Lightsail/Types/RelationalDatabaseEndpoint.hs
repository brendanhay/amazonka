-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
  ( RelationalDatabaseEndpoint (..),

    -- * Smart constructor
    mkRelationalDatabaseEndpoint,

    -- * Lenses
    rdeAddress,
    rdePort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an endpoint for a database.
--
-- /See:/ 'mkRelationalDatabaseEndpoint' smart constructor.
data RelationalDatabaseEndpoint = RelationalDatabaseEndpoint'
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

-- | Creates a value of 'RelationalDatabaseEndpoint' with the minimum fields required to make a request.
--
-- * 'address' - Specifies the DNS address of the database.
-- * 'port' - Specifies the port that the database is listening on.
mkRelationalDatabaseEndpoint ::
  RelationalDatabaseEndpoint
mkRelationalDatabaseEndpoint =
  RelationalDatabaseEndpoint'
    { address = Lude.Nothing,
      port = Lude.Nothing
    }

-- | Specifies the DNS address of the database.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdeAddress :: Lens.Lens' RelationalDatabaseEndpoint (Lude.Maybe Lude.Text)
rdeAddress = Lens.lens (address :: RelationalDatabaseEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: RelationalDatabaseEndpoint)
{-# DEPRECATED rdeAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | Specifies the port that the database is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdePort :: Lens.Lens' RelationalDatabaseEndpoint (Lude.Maybe Lude.Int)
rdePort = Lens.lens (port :: RelationalDatabaseEndpoint -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: RelationalDatabaseEndpoint)
{-# DEPRECATED rdePort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromJSON RelationalDatabaseEndpoint where
  parseJSON =
    Lude.withObject
      "RelationalDatabaseEndpoint"
      ( \x ->
          RelationalDatabaseEndpoint'
            Lude.<$> (x Lude..:? "address") Lude.<*> (x Lude..:? "port")
      )
