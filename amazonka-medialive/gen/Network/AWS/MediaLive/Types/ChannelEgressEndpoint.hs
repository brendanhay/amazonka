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
-- Module      : Network.AWS.MediaLive.Types.ChannelEgressEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelEgressEndpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Placeholder documentation for ChannelEgressEndpoint
--
-- /See:/ 'newChannelEgressEndpoint' smart constructor.
data ChannelEgressEndpoint = ChannelEgressEndpoint'
  { -- | Public IP of where a channel\'s output comes from
    sourceIp :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChannelEgressEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceIp', 'channelEgressEndpoint_sourceIp' - Public IP of where a channel\'s output comes from
newChannelEgressEndpoint ::
  ChannelEgressEndpoint
newChannelEgressEndpoint =
  ChannelEgressEndpoint' {sourceIp = Core.Nothing}

-- | Public IP of where a channel\'s output comes from
channelEgressEndpoint_sourceIp :: Lens.Lens' ChannelEgressEndpoint (Core.Maybe Core.Text)
channelEgressEndpoint_sourceIp = Lens.lens (\ChannelEgressEndpoint' {sourceIp} -> sourceIp) (\s@ChannelEgressEndpoint' {} a -> s {sourceIp = a} :: ChannelEgressEndpoint)

instance Core.FromJSON ChannelEgressEndpoint where
  parseJSON =
    Core.withObject
      "ChannelEgressEndpoint"
      ( \x ->
          ChannelEgressEndpoint'
            Core.<$> (x Core..:? "sourceIp")
      )

instance Core.Hashable ChannelEgressEndpoint

instance Core.NFData ChannelEgressEndpoint
