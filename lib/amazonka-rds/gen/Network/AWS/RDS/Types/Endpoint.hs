{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Endpoint
  ( Endpoint (..),

    -- * Smart constructor
    mkEndpoint,

    -- * Lenses
    eHostedZoneId,
    eAddress,
    ePort,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type represents the information you need to connect to an Amazon RDS DB instance. This data type is used as a response element in the following actions:
--
--
--     * @CreateDBInstance@
--
--
--     * @DescribeDBInstances@
--
--
--     * @DeleteDBInstance@
--
--
-- For the data structure that represents Amazon Aurora DB cluster endpoints, see @DBClusterEndpoint@ .
--
-- /See:/ 'mkEndpoint' smart constructor.
data Endpoint = Endpoint'
  { hostedZoneId :: Lude.Maybe Lude.Text,
    address :: Lude.Maybe Lude.Text,
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
-- * 'address' - Specifies the DNS address of the DB instance.
-- * 'hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
-- * 'port' - Specifies the port that the database engine is listening on.
mkEndpoint ::
  Endpoint
mkEndpoint =
  Endpoint'
    { hostedZoneId = Lude.Nothing,
      address = Lude.Nothing,
      port = Lude.Nothing
    }

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eHostedZoneId :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eHostedZoneId = Lens.lens (hostedZoneId :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {hostedZoneId = a} :: Endpoint)
{-# DEPRECATED eHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | Specifies the DNS address of the DB instance.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAddress :: Lens.Lens' Endpoint (Lude.Maybe Lude.Text)
eAddress = Lens.lens (address :: Endpoint -> Lude.Maybe Lude.Text) (\s a -> s {address = a} :: Endpoint)
{-# DEPRECATED eAddress "Use generic-lens or generic-optics with 'address' instead." #-}

-- | Specifies the port that the database engine is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ePort :: Lens.Lens' Endpoint (Lude.Maybe Lude.Int)
ePort = Lens.lens (port :: Endpoint -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: Endpoint)
{-# DEPRECATED ePort "Use generic-lens or generic-optics with 'port' instead." #-}

instance Lude.FromXML Endpoint where
  parseXML x =
    Endpoint'
      Lude.<$> (x Lude..@? "HostedZoneId")
      Lude.<*> (x Lude..@? "Address")
      Lude.<*> (x Lude..@? "Port")
