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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Nimble.Types.LaunchProfileState
import Amazonka.Nimble.Types.LaunchProfileStatusCode
import Amazonka.Nimble.Types.StreamConfiguration
import Amazonka.Nimble.Types.ValidationResult
import qualified Amazonka.Prelude as Prelude

-- | A launch profile controls your artist workforce’s access to studio
-- components, like compute farms, shared file systems, managed file
-- systems, and license server configurations, as well as instance types
-- and Amazon Machine Images (AMIs).
--
-- >  <p>Studio administrators create launch profiles in the Nimble Studio console. Artists can use their launch profiles to launch an instance from the Nimble Studio portal. Each user’s launch profile defines how they can launch a streaming session. By default, studio admins can use all launch profiles.</p>
--
-- /See:/ 'newLaunchProfile' smart constructor.
data LaunchProfile = LaunchProfile'
  { -- | Unique identifiers for a collection of EC2 subnets.
    ec2SubnetIds :: Prelude.Maybe [Prelude.Text],
    -- | A collection of labels, in the form of key:value pairs, that apply to
    -- this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The launch profile ID.
    launchProfileId :: Prelude.Maybe Prelude.Text,
    -- | Unique identifiers for a collection of studio components that can be
    -- used with this launch profile.
    studioComponentIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A friendly name for the launch profile.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The user ID of the user that most recently updated the resource.
    updatedBy :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state.
    state :: Prelude.Maybe LaunchProfileState,
    -- | A human-readable description of the launch profile.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A configuration for a streaming session.
    streamConfiguration :: Prelude.Maybe StreamConfiguration,
    -- | The list of the latest validation results.
    validationResults :: Prelude.Maybe [ValidationResult],
    -- | The version number of the protocol that is used by the launch profile.
    -- The only valid version is \"2021-03-31\".
    launchProfileProtocolVersions :: Prelude.Maybe [Prelude.Text],
    -- | The status code.
    statusCode :: Prelude.Maybe LaunchProfileStatusCode,
    -- | The user ID of the user that created the launch profile.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The status message for the launch profile.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Unix epoch timestamp in seconds for when the resource was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Unix epoch timestamp in seconds for when the resource was updated.
    updatedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ec2SubnetIds', 'launchProfile_ec2SubnetIds' - Unique identifiers for a collection of EC2 subnets.
--
-- 'tags', 'launchProfile_tags' - A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
--
-- 'launchProfileId', 'launchProfile_launchProfileId' - The launch profile ID.
--
-- 'studioComponentIds', 'launchProfile_studioComponentIds' - Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
--
-- 'name', 'launchProfile_name' - A friendly name for the launch profile.
--
-- 'updatedBy', 'launchProfile_updatedBy' - The user ID of the user that most recently updated the resource.
--
-- 'arn', 'launchProfile_arn' - The ARN of the resource.
--
-- 'state', 'launchProfile_state' - The current state.
--
-- 'description', 'launchProfile_description' - A human-readable description of the launch profile.
--
-- 'streamConfiguration', 'launchProfile_streamConfiguration' - A configuration for a streaming session.
--
-- 'validationResults', 'launchProfile_validationResults' - The list of the latest validation results.
--
-- 'launchProfileProtocolVersions', 'launchProfile_launchProfileProtocolVersions' - The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
--
-- 'statusCode', 'launchProfile_statusCode' - The status code.
--
-- 'createdBy', 'launchProfile_createdBy' - The user ID of the user that created the launch profile.
--
-- 'statusMessage', 'launchProfile_statusMessage' - The status message for the launch profile.
--
-- 'createdAt', 'launchProfile_createdAt' - The Unix epoch timestamp in seconds for when the resource was created.
--
-- 'updatedAt', 'launchProfile_updatedAt' - The Unix epoch timestamp in seconds for when the resource was updated.
newLaunchProfile ::
  LaunchProfile
newLaunchProfile =
  LaunchProfile'
    { ec2SubnetIds = Prelude.Nothing,
      tags = Prelude.Nothing,
      launchProfileId = Prelude.Nothing,
      studioComponentIds = Prelude.Nothing,
      name = Prelude.Nothing,
      updatedBy = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      streamConfiguration = Prelude.Nothing,
      validationResults = Prelude.Nothing,
      launchProfileProtocolVersions = Prelude.Nothing,
      statusCode = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Unique identifiers for a collection of EC2 subnets.
launchProfile_ec2SubnetIds :: Lens.Lens' LaunchProfile (Prelude.Maybe [Prelude.Text])
launchProfile_ec2SubnetIds = Lens.lens (\LaunchProfile' {ec2SubnetIds} -> ec2SubnetIds) (\s@LaunchProfile' {} a -> s {ec2SubnetIds = a} :: LaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | A collection of labels, in the form of key:value pairs, that apply to
-- this resource.
launchProfile_tags :: Lens.Lens' LaunchProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
launchProfile_tags = Lens.lens (\LaunchProfile' {tags} -> tags) (\s@LaunchProfile' {} a -> s {tags = a} :: LaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | The launch profile ID.
launchProfile_launchProfileId :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_launchProfileId = Lens.lens (\LaunchProfile' {launchProfileId} -> launchProfileId) (\s@LaunchProfile' {} a -> s {launchProfileId = a} :: LaunchProfile)

-- | Unique identifiers for a collection of studio components that can be
-- used with this launch profile.
launchProfile_studioComponentIds :: Lens.Lens' LaunchProfile (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
launchProfile_studioComponentIds = Lens.lens (\LaunchProfile' {studioComponentIds} -> studioComponentIds) (\s@LaunchProfile' {} a -> s {studioComponentIds = a} :: LaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | A friendly name for the launch profile.
launchProfile_name :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_name = Lens.lens (\LaunchProfile' {name} -> name) (\s@LaunchProfile' {} a -> s {name = a} :: LaunchProfile) Prelude.. Lens.mapping Core._Sensitive

-- | The user ID of the user that most recently updated the resource.
launchProfile_updatedBy :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_updatedBy = Lens.lens (\LaunchProfile' {updatedBy} -> updatedBy) (\s@LaunchProfile' {} a -> s {updatedBy = a} :: LaunchProfile)

-- | The ARN of the resource.
launchProfile_arn :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_arn = Lens.lens (\LaunchProfile' {arn} -> arn) (\s@LaunchProfile' {} a -> s {arn = a} :: LaunchProfile)

-- | The current state.
launchProfile_state :: Lens.Lens' LaunchProfile (Prelude.Maybe LaunchProfileState)
launchProfile_state = Lens.lens (\LaunchProfile' {state} -> state) (\s@LaunchProfile' {} a -> s {state = a} :: LaunchProfile)

-- | A human-readable description of the launch profile.
launchProfile_description :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_description = Lens.lens (\LaunchProfile' {description} -> description) (\s@LaunchProfile' {} a -> s {description = a} :: LaunchProfile) Prelude.. Lens.mapping Core._Sensitive

-- | A configuration for a streaming session.
launchProfile_streamConfiguration :: Lens.Lens' LaunchProfile (Prelude.Maybe StreamConfiguration)
launchProfile_streamConfiguration = Lens.lens (\LaunchProfile' {streamConfiguration} -> streamConfiguration) (\s@LaunchProfile' {} a -> s {streamConfiguration = a} :: LaunchProfile)

-- | The list of the latest validation results.
launchProfile_validationResults :: Lens.Lens' LaunchProfile (Prelude.Maybe [ValidationResult])
launchProfile_validationResults = Lens.lens (\LaunchProfile' {validationResults} -> validationResults) (\s@LaunchProfile' {} a -> s {validationResults = a} :: LaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | The version number of the protocol that is used by the launch profile.
-- The only valid version is \"2021-03-31\".
launchProfile_launchProfileProtocolVersions :: Lens.Lens' LaunchProfile (Prelude.Maybe [Prelude.Text])
launchProfile_launchProfileProtocolVersions = Lens.lens (\LaunchProfile' {launchProfileProtocolVersions} -> launchProfileProtocolVersions) (\s@LaunchProfile' {} a -> s {launchProfileProtocolVersions = a} :: LaunchProfile) Prelude.. Lens.mapping Lens.coerced

-- | The status code.
launchProfile_statusCode :: Lens.Lens' LaunchProfile (Prelude.Maybe LaunchProfileStatusCode)
launchProfile_statusCode = Lens.lens (\LaunchProfile' {statusCode} -> statusCode) (\s@LaunchProfile' {} a -> s {statusCode = a} :: LaunchProfile)

-- | The user ID of the user that created the launch profile.
launchProfile_createdBy :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_createdBy = Lens.lens (\LaunchProfile' {createdBy} -> createdBy) (\s@LaunchProfile' {} a -> s {createdBy = a} :: LaunchProfile)

-- | The status message for the launch profile.
launchProfile_statusMessage :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.Text)
launchProfile_statusMessage = Lens.lens (\LaunchProfile' {statusMessage} -> statusMessage) (\s@LaunchProfile' {} a -> s {statusMessage = a} :: LaunchProfile)

-- | The Unix epoch timestamp in seconds for when the resource was created.
launchProfile_createdAt :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.UTCTime)
launchProfile_createdAt = Lens.lens (\LaunchProfile' {createdAt} -> createdAt) (\s@LaunchProfile' {} a -> s {createdAt = a} :: LaunchProfile) Prelude.. Lens.mapping Core._Time

-- | The Unix epoch timestamp in seconds for when the resource was updated.
launchProfile_updatedAt :: Lens.Lens' LaunchProfile (Prelude.Maybe Prelude.UTCTime)
launchProfile_updatedAt = Lens.lens (\LaunchProfile' {updatedAt} -> updatedAt) (\s@LaunchProfile' {} a -> s {updatedAt = a} :: LaunchProfile) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON LaunchProfile where
  parseJSON =
    Core.withObject
      "LaunchProfile"
      ( \x ->
          LaunchProfile'
            Prelude.<$> (x Core..:? "ec2SubnetIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "launchProfileId")
            Prelude.<*> (x Core..:? "studioComponentIds")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "updatedBy")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "streamConfiguration")
            Prelude.<*> ( x Core..:? "validationResults"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "launchProfileProtocolVersions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "statusCode")
            Prelude.<*> (x Core..:? "createdBy")
            Prelude.<*> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "updatedAt")
      )

instance Prelude.Hashable LaunchProfile where
  hashWithSalt _salt LaunchProfile' {..} =
    _salt `Prelude.hashWithSalt` ec2SubnetIds
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` studioComponentIds
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` updatedBy
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` streamConfiguration
      `Prelude.hashWithSalt` validationResults
      `Prelude.hashWithSalt` launchProfileProtocolVersions
      `Prelude.hashWithSalt` statusCode
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData LaunchProfile where
  rnf LaunchProfile' {..} =
    Prelude.rnf ec2SubnetIds
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf studioComponentIds
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf updatedBy
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf streamConfiguration
      `Prelude.seq` Prelude.rnf validationResults
      `Prelude.seq` Prelude.rnf launchProfileProtocolVersions
      `Prelude.seq` Prelude.rnf statusCode
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
