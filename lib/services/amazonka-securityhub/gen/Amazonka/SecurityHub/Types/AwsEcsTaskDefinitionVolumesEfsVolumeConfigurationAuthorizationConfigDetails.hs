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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails' smart constructor.
data AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails = AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails'
  { -- | The Amazon EFS access point identifier to use.
    accessPointId :: Prelude.Maybe Prelude.Text,
    -- | Whether to use the Amazon ECS task IAM role defined in a task definition
    -- when mounting the Amazon EFS file system.
    iam :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPointId', 'awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_accessPointId' - The Amazon EFS access point identifier to use.
--
-- 'iam', 'awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_iam' - Whether to use the Amazon ECS task IAM role defined in a task definition
-- when mounting the Amazon EFS file system.
newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails ::
  AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
newAwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails =
  AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails'
    { accessPointId =
        Prelude.Nothing,
      iam =
        Prelude.Nothing
    }

-- | The Amazon EFS access point identifier to use.
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_accessPointId :: Lens.Lens' AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_accessPointId = Lens.lens (\AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails' {accessPointId} -> accessPointId) (\s@AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails' {} a -> s {accessPointId = a} :: AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails)

-- | Whether to use the Amazon ECS task IAM role defined in a task definition
-- when mounting the Amazon EFS file system.
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_iam :: Lens.Lens' AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails_iam = Lens.lens (\AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails' {iam} -> iam) (\s@AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails' {} a -> s {iam = a} :: AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails"
      ( \x ->
          AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails'
            Prelude.<$> (x Data..:? "AccessPointId")
            Prelude.<*> (x Data..:? "Iam")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails' {..} =
      _salt
        `Prelude.hashWithSalt` accessPointId
        `Prelude.hashWithSalt` iam

instance
  Prelude.NFData
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
  where
  rnf
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails' {..} =
      Prelude.rnf accessPointId
        `Prelude.seq` Prelude.rnf iam

instance
  Data.ToJSON
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails
  where
  toJSON
    AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationAuthorizationConfigDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AccessPointId" Data..=) Prelude.<$> accessPointId,
              ("Iam" Data..=) Prelude.<$> iam
            ]
        )
