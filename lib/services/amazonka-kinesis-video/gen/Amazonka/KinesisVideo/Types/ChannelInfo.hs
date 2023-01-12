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
-- Module      : Amazonka.KinesisVideo.Types.ChannelInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.ChannelInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideo.Types.ChannelType
import Amazonka.KinesisVideo.Types.SingleMasterConfiguration
import Amazonka.KinesisVideo.Types.StreamStatus
import qualified Amazonka.Prelude as Prelude

-- | A structure that encapsulates a signaling channel\'s metadata and
-- properties.
--
-- /See:/ 'newChannelInfo' smart constructor.
data ChannelInfo = ChannelInfo'
  { -- | The Amazon Resource Name (ARN) of the signaling channel.
    channelARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the signaling channel.
    channelName :: Prelude.Maybe Prelude.Text,
    -- | Current status of the signaling channel.
    channelStatus :: Prelude.Maybe StreamStatus,
    -- | The type of the signaling channel.
    channelType :: Prelude.Maybe ChannelType,
    -- | The time at which the signaling channel was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A structure that contains the configuration for the @SINGLE_MASTER@
    -- channel type.
    singleMasterConfiguration :: Prelude.Maybe SingleMasterConfiguration,
    -- | The current version of the signaling channel.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelARN', 'channelInfo_channelARN' - The Amazon Resource Name (ARN) of the signaling channel.
--
-- 'channelName', 'channelInfo_channelName' - The name of the signaling channel.
--
-- 'channelStatus', 'channelInfo_channelStatus' - Current status of the signaling channel.
--
-- 'channelType', 'channelInfo_channelType' - The type of the signaling channel.
--
-- 'creationTime', 'channelInfo_creationTime' - The time at which the signaling channel was created.
--
-- 'singleMasterConfiguration', 'channelInfo_singleMasterConfiguration' - A structure that contains the configuration for the @SINGLE_MASTER@
-- channel type.
--
-- 'version', 'channelInfo_version' - The current version of the signaling channel.
newChannelInfo ::
  ChannelInfo
newChannelInfo =
  ChannelInfo'
    { channelARN = Prelude.Nothing,
      channelName = Prelude.Nothing,
      channelStatus = Prelude.Nothing,
      channelType = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      singleMasterConfiguration = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the signaling channel.
channelInfo_channelARN :: Lens.Lens' ChannelInfo (Prelude.Maybe Prelude.Text)
channelInfo_channelARN = Lens.lens (\ChannelInfo' {channelARN} -> channelARN) (\s@ChannelInfo' {} a -> s {channelARN = a} :: ChannelInfo)

-- | The name of the signaling channel.
channelInfo_channelName :: Lens.Lens' ChannelInfo (Prelude.Maybe Prelude.Text)
channelInfo_channelName = Lens.lens (\ChannelInfo' {channelName} -> channelName) (\s@ChannelInfo' {} a -> s {channelName = a} :: ChannelInfo)

-- | Current status of the signaling channel.
channelInfo_channelStatus :: Lens.Lens' ChannelInfo (Prelude.Maybe StreamStatus)
channelInfo_channelStatus = Lens.lens (\ChannelInfo' {channelStatus} -> channelStatus) (\s@ChannelInfo' {} a -> s {channelStatus = a} :: ChannelInfo)

-- | The type of the signaling channel.
channelInfo_channelType :: Lens.Lens' ChannelInfo (Prelude.Maybe ChannelType)
channelInfo_channelType = Lens.lens (\ChannelInfo' {channelType} -> channelType) (\s@ChannelInfo' {} a -> s {channelType = a} :: ChannelInfo)

-- | The time at which the signaling channel was created.
channelInfo_creationTime :: Lens.Lens' ChannelInfo (Prelude.Maybe Prelude.UTCTime)
channelInfo_creationTime = Lens.lens (\ChannelInfo' {creationTime} -> creationTime) (\s@ChannelInfo' {} a -> s {creationTime = a} :: ChannelInfo) Prelude.. Lens.mapping Data._Time

-- | A structure that contains the configuration for the @SINGLE_MASTER@
-- channel type.
channelInfo_singleMasterConfiguration :: Lens.Lens' ChannelInfo (Prelude.Maybe SingleMasterConfiguration)
channelInfo_singleMasterConfiguration = Lens.lens (\ChannelInfo' {singleMasterConfiguration} -> singleMasterConfiguration) (\s@ChannelInfo' {} a -> s {singleMasterConfiguration = a} :: ChannelInfo)

-- | The current version of the signaling channel.
channelInfo_version :: Lens.Lens' ChannelInfo (Prelude.Maybe Prelude.Text)
channelInfo_version = Lens.lens (\ChannelInfo' {version} -> version) (\s@ChannelInfo' {} a -> s {version = a} :: ChannelInfo)

instance Data.FromJSON ChannelInfo where
  parseJSON =
    Data.withObject
      "ChannelInfo"
      ( \x ->
          ChannelInfo'
            Prelude.<$> (x Data..:? "ChannelARN")
            Prelude.<*> (x Data..:? "ChannelName")
            Prelude.<*> (x Data..:? "ChannelStatus")
            Prelude.<*> (x Data..:? "ChannelType")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "SingleMasterConfiguration")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable ChannelInfo where
  hashWithSalt _salt ChannelInfo' {..} =
    _salt `Prelude.hashWithSalt` channelARN
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` channelStatus
      `Prelude.hashWithSalt` channelType
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` singleMasterConfiguration
      `Prelude.hashWithSalt` version

instance Prelude.NFData ChannelInfo where
  rnf ChannelInfo' {..} =
    Prelude.rnf channelARN
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelStatus
      `Prelude.seq` Prelude.rnf channelType
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf singleMasterConfiguration
      `Prelude.seq` Prelude.rnf version
