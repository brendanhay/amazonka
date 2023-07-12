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
-- Module      : Amazonka.MediaLive.Types.RemixSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.RemixSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.AudioChannelMapping
import qualified Amazonka.Prelude as Prelude

-- | Remix Settings
--
-- /See:/ 'newRemixSettings' smart constructor.
data RemixSettings = RemixSettings'
  { -- | Number of input channels to be used.
    channelsIn :: Prelude.Maybe Prelude.Natural,
    -- | Number of output channels to be produced. Valid values: 1, 2, 4, 6, 8
    channelsOut :: Prelude.Maybe Prelude.Natural,
    -- | Mapping of input channels to output channels, with appropriate gain
    -- adjustments.
    channelMappings :: [AudioChannelMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { channelsIn = Prelude.Nothing,
      channelsOut = Prelude.Nothing,
      channelMappings = Prelude.mempty
    }

-- | Number of input channels to be used.
remixSettings_channelsIn :: Lens.Lens' RemixSettings (Prelude.Maybe Prelude.Natural)
remixSettings_channelsIn = Lens.lens (\RemixSettings' {channelsIn} -> channelsIn) (\s@RemixSettings' {} a -> s {channelsIn = a} :: RemixSettings)

-- | Number of output channels to be produced. Valid values: 1, 2, 4, 6, 8
remixSettings_channelsOut :: Lens.Lens' RemixSettings (Prelude.Maybe Prelude.Natural)
remixSettings_channelsOut = Lens.lens (\RemixSettings' {channelsOut} -> channelsOut) (\s@RemixSettings' {} a -> s {channelsOut = a} :: RemixSettings)

-- | Mapping of input channels to output channels, with appropriate gain
-- adjustments.
remixSettings_channelMappings :: Lens.Lens' RemixSettings [AudioChannelMapping]
remixSettings_channelMappings = Lens.lens (\RemixSettings' {channelMappings} -> channelMappings) (\s@RemixSettings' {} a -> s {channelMappings = a} :: RemixSettings) Prelude.. Lens.coerced

instance Data.FromJSON RemixSettings where
  parseJSON =
    Data.withObject
      "RemixSettings"
      ( \x ->
          RemixSettings'
            Prelude.<$> (x Data..:? "channelsIn")
            Prelude.<*> (x Data..:? "channelsOut")
            Prelude.<*> ( x
                            Data..:? "channelMappings"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RemixSettings where
  hashWithSalt _salt RemixSettings' {..} =
    _salt
      `Prelude.hashWithSalt` channelsIn
      `Prelude.hashWithSalt` channelsOut
      `Prelude.hashWithSalt` channelMappings

instance Prelude.NFData RemixSettings where
  rnf RemixSettings' {..} =
    Prelude.rnf channelsIn
      `Prelude.seq` Prelude.rnf channelsOut
      `Prelude.seq` Prelude.rnf channelMappings

instance Data.ToJSON RemixSettings where
  toJSON RemixSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("channelsIn" Data..=) Prelude.<$> channelsIn,
            ("channelsOut" Data..=) Prelude.<$> channelsOut,
            Prelude.Just
              ("channelMappings" Data..= channelMappings)
          ]
      )
