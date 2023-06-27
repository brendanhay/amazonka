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
-- Module      : Amazonka.MediaPackageV2.Types.ChannelGroupListConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.ChannelGroupListConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the channel group.
--
-- /See:/ 'newChannelGroupListConfiguration' smart constructor.
data ChannelGroupListConfiguration = ChannelGroupListConfiguration'
  { -- | Any descriptive information that you want to add to the channel group
    -- for future identification purposes.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) associated with the resource.
    arn :: Prelude.Text,
    -- | The date and time the channel group was created.
    createdAt :: Data.POSIX,
    -- | The date and time the channel group was modified.
    modifiedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChannelGroupListConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'channelGroupListConfiguration_description' - Any descriptive information that you want to add to the channel group
-- for future identification purposes.
--
-- 'channelGroupName', 'channelGroupListConfiguration_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'arn', 'channelGroupListConfiguration_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'createdAt', 'channelGroupListConfiguration_createdAt' - The date and time the channel group was created.
--
-- 'modifiedAt', 'channelGroupListConfiguration_modifiedAt' - The date and time the channel group was modified.
newChannelGroupListConfiguration ::
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  ChannelGroupListConfiguration
newChannelGroupListConfiguration
  pChannelGroupName_
  pArn_
  pCreatedAt_
  pModifiedAt_ =
    ChannelGroupListConfiguration'
      { description =
          Prelude.Nothing,
        channelGroupName = pChannelGroupName_,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        modifiedAt = Data._Time Lens.# pModifiedAt_
      }

-- | Any descriptive information that you want to add to the channel group
-- for future identification purposes.
channelGroupListConfiguration_description :: Lens.Lens' ChannelGroupListConfiguration (Prelude.Maybe Prelude.Text)
channelGroupListConfiguration_description = Lens.lens (\ChannelGroupListConfiguration' {description} -> description) (\s@ChannelGroupListConfiguration' {} a -> s {description = a} :: ChannelGroupListConfiguration)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
channelGroupListConfiguration_channelGroupName :: Lens.Lens' ChannelGroupListConfiguration Prelude.Text
channelGroupListConfiguration_channelGroupName = Lens.lens (\ChannelGroupListConfiguration' {channelGroupName} -> channelGroupName) (\s@ChannelGroupListConfiguration' {} a -> s {channelGroupName = a} :: ChannelGroupListConfiguration)

-- | The Amazon Resource Name (ARN) associated with the resource.
channelGroupListConfiguration_arn :: Lens.Lens' ChannelGroupListConfiguration Prelude.Text
channelGroupListConfiguration_arn = Lens.lens (\ChannelGroupListConfiguration' {arn} -> arn) (\s@ChannelGroupListConfiguration' {} a -> s {arn = a} :: ChannelGroupListConfiguration)

-- | The date and time the channel group was created.
channelGroupListConfiguration_createdAt :: Lens.Lens' ChannelGroupListConfiguration Prelude.UTCTime
channelGroupListConfiguration_createdAt = Lens.lens (\ChannelGroupListConfiguration' {createdAt} -> createdAt) (\s@ChannelGroupListConfiguration' {} a -> s {createdAt = a} :: ChannelGroupListConfiguration) Prelude.. Data._Time

-- | The date and time the channel group was modified.
channelGroupListConfiguration_modifiedAt :: Lens.Lens' ChannelGroupListConfiguration Prelude.UTCTime
channelGroupListConfiguration_modifiedAt = Lens.lens (\ChannelGroupListConfiguration' {modifiedAt} -> modifiedAt) (\s@ChannelGroupListConfiguration' {} a -> s {modifiedAt = a} :: ChannelGroupListConfiguration) Prelude.. Data._Time

instance Data.FromJSON ChannelGroupListConfiguration where
  parseJSON =
    Data.withObject
      "ChannelGroupListConfiguration"
      ( \x ->
          ChannelGroupListConfiguration'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..: "ChannelGroupName")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "ModifiedAt")
      )

instance
  Prelude.Hashable
    ChannelGroupListConfiguration
  where
  hashWithSalt _salt ChannelGroupListConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` channelGroupName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` modifiedAt

instance Prelude.NFData ChannelGroupListConfiguration where
  rnf ChannelGroupListConfiguration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf modifiedAt
