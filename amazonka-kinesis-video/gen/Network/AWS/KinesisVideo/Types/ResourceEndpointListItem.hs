{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ResourceEndpointListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.ResourceEndpointListItem where

import Network.AWS.KinesisVideo.Types.ChannelProtocol
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that describes the endpoint of the signaling channel returned
-- by the @GetSignalingChannelEndpoint@ API.
--
-- /See:/ 'newResourceEndpointListItem' smart constructor.
data ResourceEndpointListItem = ResourceEndpointListItem'
  { -- | The endpoint of the signaling channel returned by the
    -- @GetSignalingChannelEndpoint@ API.
    resourceEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The protocol of the signaling channel returned by the
    -- @GetSignalingChannelEndpoint@ API.
    protocol :: Prelude.Maybe ChannelProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceEndpointListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceEndpoint', 'resourceEndpointListItem_resourceEndpoint' - The endpoint of the signaling channel returned by the
-- @GetSignalingChannelEndpoint@ API.
--
-- 'protocol', 'resourceEndpointListItem_protocol' - The protocol of the signaling channel returned by the
-- @GetSignalingChannelEndpoint@ API.
newResourceEndpointListItem ::
  ResourceEndpointListItem
newResourceEndpointListItem =
  ResourceEndpointListItem'
    { resourceEndpoint =
        Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The endpoint of the signaling channel returned by the
-- @GetSignalingChannelEndpoint@ API.
resourceEndpointListItem_resourceEndpoint :: Lens.Lens' ResourceEndpointListItem (Prelude.Maybe Prelude.Text)
resourceEndpointListItem_resourceEndpoint = Lens.lens (\ResourceEndpointListItem' {resourceEndpoint} -> resourceEndpoint) (\s@ResourceEndpointListItem' {} a -> s {resourceEndpoint = a} :: ResourceEndpointListItem)

-- | The protocol of the signaling channel returned by the
-- @GetSignalingChannelEndpoint@ API.
resourceEndpointListItem_protocol :: Lens.Lens' ResourceEndpointListItem (Prelude.Maybe ChannelProtocol)
resourceEndpointListItem_protocol = Lens.lens (\ResourceEndpointListItem' {protocol} -> protocol) (\s@ResourceEndpointListItem' {} a -> s {protocol = a} :: ResourceEndpointListItem)

instance Prelude.FromJSON ResourceEndpointListItem where
  parseJSON =
    Prelude.withObject
      "ResourceEndpointListItem"
      ( \x ->
          ResourceEndpointListItem'
            Prelude.<$> (x Prelude..:? "ResourceEndpoint")
            Prelude.<*> (x Prelude..:? "Protocol")
      )

instance Prelude.Hashable ResourceEndpointListItem

instance Prelude.NFData ResourceEndpointListItem
