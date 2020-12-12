{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.NetworkAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.NetworkAccessConfiguration
  ( NetworkAccessConfiguration (..),

    -- * Smart constructor
    mkNetworkAccessConfiguration,

    -- * Lenses
    nacEniId,
    nacEniPrivateIPAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the network details of the fleet or image builder instance.
--
-- /See:/ 'mkNetworkAccessConfiguration' smart constructor.
data NetworkAccessConfiguration = NetworkAccessConfiguration'
  { eniId ::
      Lude.Maybe Lude.Text,
    eniPrivateIPAddress ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkAccessConfiguration' with the minimum fields required to make a request.
--
-- * 'eniId' - The resource identifier of the elastic network interface that is attached to instances in your VPC. All network interfaces have the eni-xxxxxxxx resource identifier.
-- * 'eniPrivateIPAddress' - The private IP address of the elastic network interface that is attached to instances in your VPC.
mkNetworkAccessConfiguration ::
  NetworkAccessConfiguration
mkNetworkAccessConfiguration =
  NetworkAccessConfiguration'
    { eniId = Lude.Nothing,
      eniPrivateIPAddress = Lude.Nothing
    }

-- | The resource identifier of the elastic network interface that is attached to instances in your VPC. All network interfaces have the eni-xxxxxxxx resource identifier.
--
-- /Note:/ Consider using 'eniId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nacEniId :: Lens.Lens' NetworkAccessConfiguration (Lude.Maybe Lude.Text)
nacEniId = Lens.lens (eniId :: NetworkAccessConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {eniId = a} :: NetworkAccessConfiguration)
{-# DEPRECATED nacEniId "Use generic-lens or generic-optics with 'eniId' instead." #-}

-- | The private IP address of the elastic network interface that is attached to instances in your VPC.
--
-- /Note:/ Consider using 'eniPrivateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nacEniPrivateIPAddress :: Lens.Lens' NetworkAccessConfiguration (Lude.Maybe Lude.Text)
nacEniPrivateIPAddress = Lens.lens (eniPrivateIPAddress :: NetworkAccessConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {eniPrivateIPAddress = a} :: NetworkAccessConfiguration)
{-# DEPRECATED nacEniPrivateIPAddress "Use generic-lens or generic-optics with 'eniPrivateIPAddress' instead." #-}

instance Lude.FromJSON NetworkAccessConfiguration where
  parseJSON =
    Lude.withObject
      "NetworkAccessConfiguration"
      ( \x ->
          NetworkAccessConfiguration'
            Lude.<$> (x Lude..:? "EniId") Lude.<*> (x Lude..:? "EniPrivateIpAddress")
      )
