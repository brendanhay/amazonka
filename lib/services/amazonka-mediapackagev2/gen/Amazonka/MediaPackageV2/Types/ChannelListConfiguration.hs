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
-- Module      : Amazonka.MediaPackageV2.Types.ChannelListConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.ChannelListConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the channel.
--
-- /See:/ 'newChannelListConfiguration' smart constructor.
data ChannelListConfiguration = ChannelListConfiguration'
  { -- | Any descriptive information that you want to add to the channel for
    -- future identification purposes.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) associated with the resource.
    arn :: Prelude.Text,
    -- | The name that describes the channel. The name is the primary identifier
    -- for the channel, and must be unique for your account in the AWS Region
    -- and channel group.
    channelName :: Prelude.Text,
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The date and time the channel was created.
    createdAt :: Data.POSIX,
    -- | The date and time the channel was modified.
    modifiedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelListConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'channelListConfiguration_description' - Any descriptive information that you want to add to the channel for
-- future identification purposes.
--
-- 'arn', 'channelListConfiguration_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'channelName', 'channelListConfiguration_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'channelGroupName', 'channelListConfiguration_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'createdAt', 'channelListConfiguration_createdAt' - The date and time the channel was created.
--
-- 'modifiedAt', 'channelListConfiguration_modifiedAt' - The date and time the channel was modified.
newChannelListConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  ChannelListConfiguration
newChannelListConfiguration
  pArn_
  pChannelName_
  pChannelGroupName_
  pCreatedAt_
  pModifiedAt_ =
    ChannelListConfiguration'
      { description =
          Prelude.Nothing,
        arn = pArn_,
        channelName = pChannelName_,
        channelGroupName = pChannelGroupName_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        modifiedAt = Data._Time Lens.# pModifiedAt_
      }

-- | Any descriptive information that you want to add to the channel for
-- future identification purposes.
channelListConfiguration_description :: Lens.Lens' ChannelListConfiguration (Prelude.Maybe Prelude.Text)
channelListConfiguration_description = Lens.lens (\ChannelListConfiguration' {description} -> description) (\s@ChannelListConfiguration' {} a -> s {description = a} :: ChannelListConfiguration)

-- | The Amazon Resource Name (ARN) associated with the resource.
channelListConfiguration_arn :: Lens.Lens' ChannelListConfiguration Prelude.Text
channelListConfiguration_arn = Lens.lens (\ChannelListConfiguration' {arn} -> arn) (\s@ChannelListConfiguration' {} a -> s {arn = a} :: ChannelListConfiguration)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
channelListConfiguration_channelName :: Lens.Lens' ChannelListConfiguration Prelude.Text
channelListConfiguration_channelName = Lens.lens (\ChannelListConfiguration' {channelName} -> channelName) (\s@ChannelListConfiguration' {} a -> s {channelName = a} :: ChannelListConfiguration)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
channelListConfiguration_channelGroupName :: Lens.Lens' ChannelListConfiguration Prelude.Text
channelListConfiguration_channelGroupName = Lens.lens (\ChannelListConfiguration' {channelGroupName} -> channelGroupName) (\s@ChannelListConfiguration' {} a -> s {channelGroupName = a} :: ChannelListConfiguration)

-- | The date and time the channel was created.
channelListConfiguration_createdAt :: Lens.Lens' ChannelListConfiguration Prelude.UTCTime
channelListConfiguration_createdAt = Lens.lens (\ChannelListConfiguration' {createdAt} -> createdAt) (\s@ChannelListConfiguration' {} a -> s {createdAt = a} :: ChannelListConfiguration) Prelude.. Data._Time

-- | The date and time the channel was modified.
channelListConfiguration_modifiedAt :: Lens.Lens' ChannelListConfiguration Prelude.UTCTime
channelListConfiguration_modifiedAt = Lens.lens (\ChannelListConfiguration' {modifiedAt} -> modifiedAt) (\s@ChannelListConfiguration' {} a -> s {modifiedAt = a} :: ChannelListConfiguration) Prelude.. Data._Time

instance Data.FromJSON ChannelListConfiguration where
  parseJSON =
    Data.withObject
      "ChannelListConfiguration"
      ( \x ->
          ChannelListConfiguration'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "ChannelName")
            Prelude.<*> (x Data..: "ChannelGroupName")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "ModifiedAt")
      )

instance Prelude.Hashable ChannelListConfiguration where
  hashWithSalt _salt ChannelListConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` modifiedAt

instance Prelude.NFData ChannelListConfiguration where
  rnf ChannelListConfiguration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
