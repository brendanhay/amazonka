{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ResourceEndpointListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.ResourceEndpointListItem
  ( ResourceEndpointListItem (..),

    -- * Smart constructor
    mkResourceEndpointListItem,

    -- * Lenses
    reliProtocol,
    reliResourceEndpoint,
  )
where

import Network.AWS.KinesisVideo.Types.ChannelProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that describes the endpoint of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
--
-- /See:/ 'mkResourceEndpointListItem' smart constructor.
data ResourceEndpointListItem = ResourceEndpointListItem'
  { -- | The protocol of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
    protocol :: Lude.Maybe ChannelProtocol,
    -- | The endpoint of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
    resourceEndpoint :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceEndpointListItem' with the minimum fields required to make a request.
--
-- * 'protocol' - The protocol of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
-- * 'resourceEndpoint' - The endpoint of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
mkResourceEndpointListItem ::
  ResourceEndpointListItem
mkResourceEndpointListItem =
  ResourceEndpointListItem'
    { protocol = Lude.Nothing,
      resourceEndpoint = Lude.Nothing
    }

-- | The protocol of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reliProtocol :: Lens.Lens' ResourceEndpointListItem (Lude.Maybe ChannelProtocol)
reliProtocol = Lens.lens (protocol :: ResourceEndpointListItem -> Lude.Maybe ChannelProtocol) (\s a -> s {protocol = a} :: ResourceEndpointListItem)
{-# DEPRECATED reliProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The endpoint of the signaling channel returned by the @GetSignalingChannelEndpoint@ API.
--
-- /Note:/ Consider using 'resourceEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reliResourceEndpoint :: Lens.Lens' ResourceEndpointListItem (Lude.Maybe Lude.Text)
reliResourceEndpoint = Lens.lens (resourceEndpoint :: ResourceEndpointListItem -> Lude.Maybe Lude.Text) (\s a -> s {resourceEndpoint = a} :: ResourceEndpointListItem)
{-# DEPRECATED reliResourceEndpoint "Use generic-lens or generic-optics with 'resourceEndpoint' instead." #-}

instance Lude.FromJSON ResourceEndpointListItem where
  parseJSON =
    Lude.withObject
      "ResourceEndpointListItem"
      ( \x ->
          ResourceEndpointListItem'
            Lude.<$> (x Lude..:? "Protocol") Lude.<*> (x Lude..:? "ResourceEndpoint")
      )
