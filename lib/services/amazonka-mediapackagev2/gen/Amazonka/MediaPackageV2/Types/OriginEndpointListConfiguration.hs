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
-- Module      : Amazonka.MediaPackageV2.Types.OriginEndpointListConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.OriginEndpointListConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.ContainerType
import Amazonka.MediaPackageV2.Types.ListHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.ListLowLatencyHlsManifestConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the origin endpoint.
--
-- /See:/ 'newOriginEndpointListConfiguration' smart constructor.
data OriginEndpointListConfiguration = OriginEndpointListConfiguration'
  { -- | The date and time the origin endpoint was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Any descriptive information that you want to add to the origin endpoint
    -- for future identification purposes.
    description :: Prelude.Maybe Prelude.Text,
    -- | An HTTP live streaming (HLS) manifest configuration.
    hlsManifests :: Prelude.Maybe [ListHlsManifestConfiguration],
    -- | A low-latency HLS manifest configuration.
    lowLatencyHlsManifests :: Prelude.Maybe [ListLowLatencyHlsManifestConfiguration],
    -- | The date and time the origin endpoint was modified.
    modifiedAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) associated with the resource.
    arn :: Prelude.Text,
    -- | The name that describes the channel group. The name is the primary
    -- identifier for the channel group, and must be unique for your account in
    -- the AWS Region.
    channelGroupName :: Prelude.Text,
    -- | The name that describes the channel. The name is the primary identifier
    -- for the channel, and must be unique for your account in the AWS Region
    -- and channel group.
    channelName :: Prelude.Text,
    -- | The name that describes the origin endpoint. The name is the primary
    -- identifier for the origin endpoint, and and must be unique for your
    -- account in the AWS Region and channel.
    originEndpointName :: Prelude.Text,
    -- | The type of container attached to this origin endpoint. A container type
    -- is a file format that encapsulates one or more media streams, such as
    -- audio and video, into a single file.
    containerType :: ContainerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginEndpointListConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'originEndpointListConfiguration_createdAt' - The date and time the origin endpoint was created.
--
-- 'description', 'originEndpointListConfiguration_description' - Any descriptive information that you want to add to the origin endpoint
-- for future identification purposes.
--
-- 'hlsManifests', 'originEndpointListConfiguration_hlsManifests' - An HTTP live streaming (HLS) manifest configuration.
--
-- 'lowLatencyHlsManifests', 'originEndpointListConfiguration_lowLatencyHlsManifests' - A low-latency HLS manifest configuration.
--
-- 'modifiedAt', 'originEndpointListConfiguration_modifiedAt' - The date and time the origin endpoint was modified.
--
-- 'arn', 'originEndpointListConfiguration_arn' - The Amazon Resource Name (ARN) associated with the resource.
--
-- 'channelGroupName', 'originEndpointListConfiguration_channelGroupName' - The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
--
-- 'channelName', 'originEndpointListConfiguration_channelName' - The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
--
-- 'originEndpointName', 'originEndpointListConfiguration_originEndpointName' - The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
--
-- 'containerType', 'originEndpointListConfiguration_containerType' - The type of container attached to this origin endpoint. A container type
-- is a file format that encapsulates one or more media streams, such as
-- audio and video, into a single file.
newOriginEndpointListConfiguration ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'channelGroupName'
  Prelude.Text ->
  -- | 'channelName'
  Prelude.Text ->
  -- | 'originEndpointName'
  Prelude.Text ->
  -- | 'containerType'
  ContainerType ->
  OriginEndpointListConfiguration
newOriginEndpointListConfiguration
  pArn_
  pChannelGroupName_
  pChannelName_
  pOriginEndpointName_
  pContainerType_ =
    OriginEndpointListConfiguration'
      { createdAt =
          Prelude.Nothing,
        description = Prelude.Nothing,
        hlsManifests = Prelude.Nothing,
        lowLatencyHlsManifests = Prelude.Nothing,
        modifiedAt = Prelude.Nothing,
        arn = pArn_,
        channelGroupName = pChannelGroupName_,
        channelName = pChannelName_,
        originEndpointName = pOriginEndpointName_,
        containerType = pContainerType_
      }

-- | The date and time the origin endpoint was created.
originEndpointListConfiguration_createdAt :: Lens.Lens' OriginEndpointListConfiguration (Prelude.Maybe Prelude.UTCTime)
originEndpointListConfiguration_createdAt = Lens.lens (\OriginEndpointListConfiguration' {createdAt} -> createdAt) (\s@OriginEndpointListConfiguration' {} a -> s {createdAt = a} :: OriginEndpointListConfiguration) Prelude.. Lens.mapping Data._Time

-- | Any descriptive information that you want to add to the origin endpoint
-- for future identification purposes.
originEndpointListConfiguration_description :: Lens.Lens' OriginEndpointListConfiguration (Prelude.Maybe Prelude.Text)
originEndpointListConfiguration_description = Lens.lens (\OriginEndpointListConfiguration' {description} -> description) (\s@OriginEndpointListConfiguration' {} a -> s {description = a} :: OriginEndpointListConfiguration)

-- | An HTTP live streaming (HLS) manifest configuration.
originEndpointListConfiguration_hlsManifests :: Lens.Lens' OriginEndpointListConfiguration (Prelude.Maybe [ListHlsManifestConfiguration])
originEndpointListConfiguration_hlsManifests = Lens.lens (\OriginEndpointListConfiguration' {hlsManifests} -> hlsManifests) (\s@OriginEndpointListConfiguration' {} a -> s {hlsManifests = a} :: OriginEndpointListConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A low-latency HLS manifest configuration.
originEndpointListConfiguration_lowLatencyHlsManifests :: Lens.Lens' OriginEndpointListConfiguration (Prelude.Maybe [ListLowLatencyHlsManifestConfiguration])
originEndpointListConfiguration_lowLatencyHlsManifests = Lens.lens (\OriginEndpointListConfiguration' {lowLatencyHlsManifests} -> lowLatencyHlsManifests) (\s@OriginEndpointListConfiguration' {} a -> s {lowLatencyHlsManifests = a} :: OriginEndpointListConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the origin endpoint was modified.
originEndpointListConfiguration_modifiedAt :: Lens.Lens' OriginEndpointListConfiguration (Prelude.Maybe Prelude.UTCTime)
originEndpointListConfiguration_modifiedAt = Lens.lens (\OriginEndpointListConfiguration' {modifiedAt} -> modifiedAt) (\s@OriginEndpointListConfiguration' {} a -> s {modifiedAt = a} :: OriginEndpointListConfiguration) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) associated with the resource.
originEndpointListConfiguration_arn :: Lens.Lens' OriginEndpointListConfiguration Prelude.Text
originEndpointListConfiguration_arn = Lens.lens (\OriginEndpointListConfiguration' {arn} -> arn) (\s@OriginEndpointListConfiguration' {} a -> s {arn = a} :: OriginEndpointListConfiguration)

-- | The name that describes the channel group. The name is the primary
-- identifier for the channel group, and must be unique for your account in
-- the AWS Region.
originEndpointListConfiguration_channelGroupName :: Lens.Lens' OriginEndpointListConfiguration Prelude.Text
originEndpointListConfiguration_channelGroupName = Lens.lens (\OriginEndpointListConfiguration' {channelGroupName} -> channelGroupName) (\s@OriginEndpointListConfiguration' {} a -> s {channelGroupName = a} :: OriginEndpointListConfiguration)

-- | The name that describes the channel. The name is the primary identifier
-- for the channel, and must be unique for your account in the AWS Region
-- and channel group.
originEndpointListConfiguration_channelName :: Lens.Lens' OriginEndpointListConfiguration Prelude.Text
originEndpointListConfiguration_channelName = Lens.lens (\OriginEndpointListConfiguration' {channelName} -> channelName) (\s@OriginEndpointListConfiguration' {} a -> s {channelName = a} :: OriginEndpointListConfiguration)

-- | The name that describes the origin endpoint. The name is the primary
-- identifier for the origin endpoint, and and must be unique for your
-- account in the AWS Region and channel.
originEndpointListConfiguration_originEndpointName :: Lens.Lens' OriginEndpointListConfiguration Prelude.Text
originEndpointListConfiguration_originEndpointName = Lens.lens (\OriginEndpointListConfiguration' {originEndpointName} -> originEndpointName) (\s@OriginEndpointListConfiguration' {} a -> s {originEndpointName = a} :: OriginEndpointListConfiguration)

-- | The type of container attached to this origin endpoint. A container type
-- is a file format that encapsulates one or more media streams, such as
-- audio and video, into a single file.
originEndpointListConfiguration_containerType :: Lens.Lens' OriginEndpointListConfiguration ContainerType
originEndpointListConfiguration_containerType = Lens.lens (\OriginEndpointListConfiguration' {containerType} -> containerType) (\s@OriginEndpointListConfiguration' {} a -> s {containerType = a} :: OriginEndpointListConfiguration)

instance
  Data.FromJSON
    OriginEndpointListConfiguration
  where
  parseJSON =
    Data.withObject
      "OriginEndpointListConfiguration"
      ( \x ->
          OriginEndpointListConfiguration'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "HlsManifests" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "LowLatencyHlsManifests"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ModifiedAt")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "ChannelGroupName")
            Prelude.<*> (x Data..: "ChannelName")
            Prelude.<*> (x Data..: "OriginEndpointName")
            Prelude.<*> (x Data..: "ContainerType")
      )

instance
  Prelude.Hashable
    OriginEndpointListConfiguration
  where
  hashWithSalt
    _salt
    OriginEndpointListConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` hlsManifests
        `Prelude.hashWithSalt` lowLatencyHlsManifests
        `Prelude.hashWithSalt` modifiedAt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` channelGroupName
        `Prelude.hashWithSalt` channelName
        `Prelude.hashWithSalt` originEndpointName
        `Prelude.hashWithSalt` containerType

instance
  Prelude.NFData
    OriginEndpointListConfiguration
  where
  rnf OriginEndpointListConfiguration' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf hlsManifests
      `Prelude.seq` Prelude.rnf lowLatencyHlsManifests
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf channelGroupName
      `Prelude.seq` Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf originEndpointName
      `Prelude.seq` Prelude.rnf containerType
