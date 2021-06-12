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
-- Module      : Network.AWS.MediaLive.Types.RemixSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.RemixSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioChannelMapping

-- | Remix Settings
--
-- /See:/ 'newRemixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { -- | Number of input channels to be used.
    channelsIn :: Core.Maybe Core.Natural,
    -- | Number of output channels to be produced. Valid values: 1, 2, 4, 6, 8
    channelsOut :: Core.Maybe Core.Natural,
    -- | Mapping of input channels to output channels, with appropriate gain
    -- adjustments.
    channelMappings :: [AudioChannelMapping]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemixSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelsIn', 'remixSettings_channelsIn' - Number of input channels to be used.
--
-- 'channelsOut', 'remixSettings_channelsOut' - Number of output channels to be produced. Valid values: 1, 2, 4, 6, 8
--
-- 'channelMappings', 'remixSettings_channelMappings' - Mapping of input channels to output channels, with appropriate gain
-- adjustments.
newRemixSettings ::
  RemixSettings
newRemixSettings =
  RemixSettings'
    { channelsIn = Core.Nothing,
      channelsOut = Core.Nothing,
      channelMappings = Core.mempty
    }

-- | Number of input channels to be used.
remixSettings_channelsIn :: Lens.Lens' RemixSettings (Core.Maybe Core.Natural)
remixSettings_channelsIn = Lens.lens (\RemixSettings' {channelsIn} -> channelsIn) (\s@RemixSettings' {} a -> s {channelsIn = a} :: RemixSettings)

-- | Number of output channels to be produced. Valid values: 1, 2, 4, 6, 8
remixSettings_channelsOut :: Lens.Lens' RemixSettings (Core.Maybe Core.Natural)
remixSettings_channelsOut = Lens.lens (\RemixSettings' {channelsOut} -> channelsOut) (\s@RemixSettings' {} a -> s {channelsOut = a} :: RemixSettings)

-- | Mapping of input channels to output channels, with appropriate gain
-- adjustments.
remixSettings_channelMappings :: Lens.Lens' RemixSettings [AudioChannelMapping]
remixSettings_channelMappings = Lens.lens (\RemixSettings' {channelMappings} -> channelMappings) (\s@RemixSettings' {} a -> s {channelMappings = a} :: RemixSettings) Core.. Lens._Coerce

instance Core.FromJSON RemixSettings where
  parseJSON =
    Core.withObject
      "RemixSettings"
      ( \x ->
          RemixSettings'
            Core.<$> (x Core..:? "channelsIn")
            Core.<*> (x Core..:? "channelsOut")
            Core.<*> (x Core..:? "channelMappings" Core..!= Core.mempty)
      )

instance Core.Hashable RemixSettings

instance Core.NFData RemixSettings

instance Core.ToJSON RemixSettings where
  toJSON RemixSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("channelsIn" Core..=) Core.<$> channelsIn,
            ("channelsOut" Core..=) Core.<$> channelsOut,
            Core.Just
              ("channelMappings" Core..= channelMappings)
          ]
      )
