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
-- Module      : Amazonka.KinesisVideo.Types.ResourceEndpointListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.ResourceEndpointListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisVideo.Types.ChannelProtocol
import qualified Amazonka.Prelude as Prelude

-- | An object that describes the endpoint of the signaling channel returned
-- by the @GetSignalingChannelEndpoint@ API.
--
-- /See:/ 'newResourceEndpointListItem' smart constructor.
data ResourceEndpointListItem = ResourceEndpointListItem'
  { -- | The protocol of the signaling channel returned by the
    -- @GetSignalingChannelEndpoint@ API.
    protocol :: Prelude.Maybe ChannelProtocol,
    -- | The endpoint of the signaling channel returned by the
    -- @GetSignalingChannelEndpoint@ API.
    resourceEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceEndpointListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'resourceEndpointListItem_protocol' - The protocol of the signaling channel returned by the
-- @GetSignalingChannelEndpoint@ API.
--
-- 'resourceEndpoint', 'resourceEndpointListItem_resourceEndpoint' - The endpoint of the signaling channel returned by the
-- @GetSignalingChannelEndpoint@ API.
newResourceEndpointListItem ::
  ResourceEndpointListItem
newResourceEndpointListItem =
  ResourceEndpointListItem'
    { protocol =
        Prelude.Nothing,
      resourceEndpoint = Prelude.Nothing
    }

-- | The protocol of the signaling channel returned by the
-- @GetSignalingChannelEndpoint@ API.
resourceEndpointListItem_protocol :: Lens.Lens' ResourceEndpointListItem (Prelude.Maybe ChannelProtocol)
resourceEndpointListItem_protocol = Lens.lens (\ResourceEndpointListItem' {protocol} -> protocol) (\s@ResourceEndpointListItem' {} a -> s {protocol = a} :: ResourceEndpointListItem)

-- | The endpoint of the signaling channel returned by the
-- @GetSignalingChannelEndpoint@ API.
resourceEndpointListItem_resourceEndpoint :: Lens.Lens' ResourceEndpointListItem (Prelude.Maybe Prelude.Text)
resourceEndpointListItem_resourceEndpoint = Lens.lens (\ResourceEndpointListItem' {resourceEndpoint} -> resourceEndpoint) (\s@ResourceEndpointListItem' {} a -> s {resourceEndpoint = a} :: ResourceEndpointListItem)

instance Core.FromJSON ResourceEndpointListItem where
  parseJSON =
    Core.withObject
      "ResourceEndpointListItem"
      ( \x ->
          ResourceEndpointListItem'
            Prelude.<$> (x Core..:? "Protocol")
            Prelude.<*> (x Core..:? "ResourceEndpoint")
      )

instance Prelude.Hashable ResourceEndpointListItem where
  hashWithSalt _salt ResourceEndpointListItem' {..} =
    _salt `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` resourceEndpoint

instance Prelude.NFData ResourceEndpointListItem where
  rnf ResourceEndpointListItem' {..} =
    Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf resourceEndpoint
