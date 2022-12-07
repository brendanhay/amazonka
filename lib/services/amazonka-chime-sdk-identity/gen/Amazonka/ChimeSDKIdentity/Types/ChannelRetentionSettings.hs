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
-- Module      : Amazonka.ChimeSDKIdentity.Types.ChannelRetentionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.ChannelRetentionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the retention settings for a channel.
--
-- /See:/ 'newChannelRetentionSettings' smart constructor.
data ChannelRetentionSettings = ChannelRetentionSettings'
  { -- | The time in days to retain the messages in a channel.
    retentionDays :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelRetentionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionDays', 'channelRetentionSettings_retentionDays' - The time in days to retain the messages in a channel.
newChannelRetentionSettings ::
  ChannelRetentionSettings
newChannelRetentionSettings =
  ChannelRetentionSettings'
    { retentionDays =
        Prelude.Nothing
    }

-- | The time in days to retain the messages in a channel.
channelRetentionSettings_retentionDays :: Lens.Lens' ChannelRetentionSettings (Prelude.Maybe Prelude.Natural)
channelRetentionSettings_retentionDays = Lens.lens (\ChannelRetentionSettings' {retentionDays} -> retentionDays) (\s@ChannelRetentionSettings' {} a -> s {retentionDays = a} :: ChannelRetentionSettings)

instance Data.FromJSON ChannelRetentionSettings where
  parseJSON =
    Data.withObject
      "ChannelRetentionSettings"
      ( \x ->
          ChannelRetentionSettings'
            Prelude.<$> (x Data..:? "RetentionDays")
      )

instance Prelude.Hashable ChannelRetentionSettings where
  hashWithSalt _salt ChannelRetentionSettings' {..} =
    _salt `Prelude.hashWithSalt` retentionDays

instance Prelude.NFData ChannelRetentionSettings where
  rnf ChannelRetentionSettings' {..} =
    Prelude.rnf retentionDays

instance Data.ToJSON ChannelRetentionSettings where
  toJSON ChannelRetentionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RetentionDays" Data..=)
              Prelude.<$> retentionDays
          ]
      )
