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
-- Module      : Amazonka.Nimble.Types.LaunchProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types.LaunchProfileState
import Amazonka.Nimble.Types.LaunchProfileStatusCode
import Amazonka.Nimble.Types.StreamConfiguration
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newLaunchProfile' smart constructor.
data LaunchProfile = LaunchProfile'
  { -- | The current state.
    state :: Prelude.Maybe LaunchProfileState,
    -- | The ARN of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The user ID of the user that created the launch profile.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The launch profile ID.
    launchProfileId :: Prelude.Maybe Prelude.Text,
    -- | The user ID of the user that most recently updated the resource.
    updatedBy :: Prelude.Maybe Prelude.Text,
    -- | The version number of the protocol that is used by the launch profile.
    -- The only valid version is \"2021-03-31\".
    launchProfileProtocolVersions :: Prelude.Maybe [Prelude.Text],
    -- | Unique identifiers for a collection of EC2 subnets.
    ec2SubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | A configuration for a streaming session.
    streamConfiguration :: Prelude.Maybe StreamConfiguration,
    -- | A friendly name for the launch profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status message for the launch profile.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | A human-readable description of the launch profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The status code.
    statusCode :: Prelude.Maybe LaunchProfileStatusCode,
    -- | Unique identifiers for a collection of studio components that can be
    -- used with this launch profile.
    studioComponentIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'launchProfile_state' - The current state.
--
-- 'arn', 'launchProfile_arn' - The ARN of the resource.
--
-- 'createdAt', 'launchProfile_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'createdBy', 'launchProfile_createdBy' - The user ID of the user that created the launch profile.
--
-- 'launchProfileId', 'launchProfile_launchProfileId' - The launch profile ID.
--
-- 'updatedBy', 'launchProfile_updatedBy' - The user ID of the user that most recently updated the resource.
--
-- 'launchProfileProtocolVersions', 'launchProfile_launchProfileProtocolVersions' - The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
--
-- 'ec2SubnetIds', 'launchProfile_ec2SubnetIds' - Unique identifiers for a collection of EC2 subnets.
--
-- 'streamConfiguration', 'launchProfile_streamConfiguration' - A configuration for a streaming session.
--
-- 'name', 'launchProfile_name' - A friendly name for the launch profile.
--
-- 'statusMessage', 'launchProfile_statusMessage' - The status message for the launch profile.
--
-- 'updatedAt', 'launchProfile_updatedAt' - The Unix epoch timestamp in seconds for when the resource was updated.
--
-- 'description', 'launchProfile_description' - A human-readable description of the launch profile.
--
-- 'tags', 'launchProfile_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'statusCode', 'launchProfile_statusCode' - The status code.
--
-- 'studioComponentIds', 'launchProfile_studioComponentIds' - Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
newLaunchProfile ::
  LaunchProfile
newLaunchProfile =
  LaunchProfile'
    { state = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      launchProfileId = Prelude.Nothing,
      updatedBy = Prelude.Nothing,
      launchProfileProtocolVersions = Prelude.Nothing,
      ec2SubnetIds = Prelude.Nothing,
      streamConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      studioComponentIds = Prelude.Nothing
    }

-- | The current state.
launchProfile_state :: Lens.Lens' LaunchProfile (Prelude.Maybe LaunchProfileState)
launchProfile_state = Lens.lens (\LaunchProfile' {state} -> state) (\s@LaunchProfile' {} a -> s {state = a} :: LaunchProfile)

-- | The ARN of the resource.
launchProfile_arn :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_arn = Lens.lens (\LaunchProfile' {arn} -> arn) (\s@LaunchProfile' {} a -> s {arn = a} :: LaunchProfile)

-- | The Unix epoch timestamp in seconds for when the resource was created.
launchProfile_createdAt :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.UTCTime)
launchProfile_createdAt = Lens.lens (\LaunchProfile' {createdAt} -> createdAt) (\s@LaunchProfile' {} a -> s {createdAt = a} :: LaunchProfile) Prelude.. Lens.mapping Core._Time

-- | The user ID of the user that created the launch profile.
launchProfile_createdBy :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_createdBy = Lens.lens (\LaunchProfile' {createdBy} -> createdBy) (\s@LaunchProfile' {} a -> s {createdBy = a} :: LaunchProfile)

-- | The launch profile ID.
launchProfile_launchProfileId :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_launchProfileId = Lens.lens (\LaunchProfile' {launchProfileId} -> launchProfileId) (\s@LaunchProfile' {} a -> s {launchProfileId = a} :: LaunchProfile)

-- | The user ID of the user that most recently updated the resource.
launchProfile_updatedBy :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_updatedBy = Lens.lens (\LaunchProfile' {updatedBy} -> updatedBy) (\s@LaunchProfile' {} a -> s {updatedBy = a} :: LaunchProfile)

-- | The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
launchProfile_launchProfileProtocolVersions :: Lens.Lens' LaunchProfile (Prelude.Maybe [Prelude.Text])
launchProfile_launchProfileProtocolVersions = Lens.lens (\LaunchProfile' {launchProfileProtocolVersions} -> launchProfileProtocolVersions) (\s@LaunchProfile' {} a -> s {launchProfileProtocolVersions = a} :: LaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | Unique identifiers for a collection of EC2 subnets.
launchProfile_ec2SubnetIds :: Lens.Lens' LaunchProfile (Prelude.Maybe [Prelude.Text])
launchProfile_ec2SubnetIds = Lens.lens (\LaunchProfile' {ec2SubnetIds} -> ec2SubnetIds) (\s@LaunchProfile' {} a -> s {ec2SubnetIds = a} :: LaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | A configuration for a streaming session.
launchProfile_streamConfiguration :: Lens.Lens' LaunchProfile (Prelude.Maybe StreamConfiguration)
launchProfile_streamConfiguration = Lens.lens (\LaunchProfile' {streamConfiguration} -> streamConfiguration) (\s@LaunchProfile' {} a -> s {streamConfiguration = a} :: LaunchProfile)

-- | A friendly name for the launch profile.
launchProfile_name :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_name = Lens.lens (\LaunchProfile' {name} -> name) (\s@LaunchProfile' {} a -> s {name = a} :: LaunchProfile)

-- | The status message for the launch profile.
launchProfile_statusMessage :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_statusMessage = Lens.lens (\LaunchProfile' {statusMessage} -> statusMessage) (\s@LaunchProfile' {} a -> s {statusMessage = a} :: LaunchProfile)

-- | The Unix epoch timestamp in seconds for when the resource was updated.
launchProfile_updatedAt :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.UTCTime)
launchProfile_updatedAt = Lens.lens (\LaunchProfile' {updatedAt} -> updatedAt) (\s@LaunchProfile' {} a -> s {updatedAt = a} :: LaunchProfile) Prelude.. Lens.mapping Core._Time

-- | A human-readable description of the launch profile.
launchProfile_description :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_description = Lens.lens (\LaunchProfile' {description} -> description) (\s@LaunchProfile' {} a -> s {description = a} :: LaunchProfile)

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
launchProfile_tags :: Lens.Lens' LaunchProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
launchProfile_tags = Lens.lens (\LaunchProfile' {tags} -> tags) (\s@LaunchProfile' {} a -> s {tags = a} :: LaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | The status code.
launchProfile_statusCode :: Lens.Lens' LaunchProfile (Prelude.Maybe LaunchProfileStatusCode)
launchProfile_statusCode = Lens.lens (\LaunchProfile' {statusCode} -> statusCode) (\s@LaunchProfile' {} a -> s {statusCode = a} :: LaunchProfile)

-- | Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
launchProfile_studioComponentIds :: Lens.Lens' LaunchProfile (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
launchProfile_studioComponentIds = Lens.lens (\LaunchProfile' {studioComponentIds} -> studioComponentIds) (\s@LaunchProfile' {} a -> s {studioComponentIds = a} :: LaunchProfile) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON LaunchProfile where
  parseJSON =
    Core.withObject
      "LaunchProfile"
      ( \x ->
          LaunchProfile'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "createdBy")
            Prelude.<*> (x Core..:? "launchProfileId")
            Prelude.<*> (x Core..:? "updatedBy")
            Prelude.<*> ( x Core..:? "launchProfileProtocolVersions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ec2SubnetIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "streamConfiguration")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "updatedAt")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "statusCode")
            Prelude.<*> (x Core..:? "studioComponentIds")
      )

instance Prelude.Hashable LaunchProfile where
  hashWithSalt _salt LaunchProfile' {..} =
    _salt `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` updatedBy
      `Prelude.hashWithSalt` launchProfileProtocolVersions
      `Prelude.hashWithSalt` ec2SubnetIds
      `Prelude.hashWithSalt` streamConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` studioComponentIds

instance Prelude.NFData LaunchProfile where
  rnf LaunchProfile' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf updatedBy
      `Prelude.seq` Prelude.rnf launchProfileProtocolVersions
      `Prelude.seq` Prelude.rnf ec2SubnetIds
      `Prelude.seq` Prelude.rnf streamConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf studioComponentIds
