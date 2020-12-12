{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ConnectivityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ConnectivityInfo
  ( ConnectivityInfo (..),

    -- * Smart constructor
    mkConnectivityInfo,

    -- * Lenses
    ciPortNumber,
    ciId,
    ciMetadata,
    ciHostAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a Greengrass core's connectivity.
--
-- /See:/ 'mkConnectivityInfo' smart constructor.
data ConnectivityInfo = ConnectivityInfo'
  { portNumber ::
      Lude.Maybe Lude.Int,
    id :: Lude.Maybe Lude.Text,
    metadata :: Lude.Maybe Lude.Text,
    hostAddress :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectivityInfo' with the minimum fields required to make a request.
--
-- * 'hostAddress' - The endpoint for the Greengrass core. Can be an IP address or DNS.
-- * 'id' - The ID of the connectivity information.
-- * 'metadata' - Metadata for this endpoint.
-- * 'portNumber' - The port of the Greengrass core. Usually 8883.
mkConnectivityInfo ::
  ConnectivityInfo
mkConnectivityInfo =
  ConnectivityInfo'
    { portNumber = Lude.Nothing,
      id = Lude.Nothing,
      metadata = Lude.Nothing,
      hostAddress = Lude.Nothing
    }

-- | The port of the Greengrass core. Usually 8883.
--
-- /Note:/ Consider using 'portNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciPortNumber :: Lens.Lens' ConnectivityInfo (Lude.Maybe Lude.Int)
ciPortNumber = Lens.lens (portNumber :: ConnectivityInfo -> Lude.Maybe Lude.Int) (\s a -> s {portNumber = a} :: ConnectivityInfo)
{-# DEPRECATED ciPortNumber "Use generic-lens or generic-optics with 'portNumber' instead." #-}

-- | The ID of the connectivity information.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciId :: Lens.Lens' ConnectivityInfo (Lude.Maybe Lude.Text)
ciId = Lens.lens (id :: ConnectivityInfo -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ConnectivityInfo)
{-# DEPRECATED ciId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Metadata for this endpoint.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciMetadata :: Lens.Lens' ConnectivityInfo (Lude.Maybe Lude.Text)
ciMetadata = Lens.lens (metadata :: ConnectivityInfo -> Lude.Maybe Lude.Text) (\s a -> s {metadata = a} :: ConnectivityInfo)
{-# DEPRECATED ciMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The endpoint for the Greengrass core. Can be an IP address or DNS.
--
-- /Note:/ Consider using 'hostAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciHostAddress :: Lens.Lens' ConnectivityInfo (Lude.Maybe Lude.Text)
ciHostAddress = Lens.lens (hostAddress :: ConnectivityInfo -> Lude.Maybe Lude.Text) (\s a -> s {hostAddress = a} :: ConnectivityInfo)
{-# DEPRECATED ciHostAddress "Use generic-lens or generic-optics with 'hostAddress' instead." #-}

instance Lude.FromJSON ConnectivityInfo where
  parseJSON =
    Lude.withObject
      "ConnectivityInfo"
      ( \x ->
          ConnectivityInfo'
            Lude.<$> (x Lude..:? "PortNumber")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Metadata")
            Lude.<*> (x Lude..:? "HostAddress")
      )

instance Lude.ToJSON ConnectivityInfo where
  toJSON ConnectivityInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PortNumber" Lude..=) Lude.<$> portNumber,
            ("Id" Lude..=) Lude.<$> id,
            ("Metadata" Lude..=) Lude.<$> metadata,
            ("HostAddress" Lude..=) Lude.<$> hostAddress
          ]
      )
