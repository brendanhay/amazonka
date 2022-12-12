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
-- Module      : Amazonka.ImageBuilder.Types.AmiDistributionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.AmiDistributionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.LaunchPermissionConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Define and configure the output AMIs of the pipeline.
--
-- /See:/ 'newAmiDistributionConfiguration' smart constructor.
data AmiDistributionConfiguration = AmiDistributionConfiguration'
  { -- | The tags to apply to AMIs distributed to this Region.
    amiTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The description of the AMI distribution configuration. Minimum and
    -- maximum length are in characters.
    description :: Prelude.Maybe Prelude.Text,
    -- | The KMS key identifier used to encrypt the distributed image.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Launch permissions can be used to configure which Amazon Web Services
    -- accounts can use the AMI to launch instances.
    launchPermission :: Prelude.Maybe LaunchPermissionConfiguration,
    -- | The name of the output AMI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of an account to which you want to distribute an image.
    targetAccountIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AmiDistributionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amiTags', 'amiDistributionConfiguration_amiTags' - The tags to apply to AMIs distributed to this Region.
--
-- 'description', 'amiDistributionConfiguration_description' - The description of the AMI distribution configuration. Minimum and
-- maximum length are in characters.
--
-- 'kmsKeyId', 'amiDistributionConfiguration_kmsKeyId' - The KMS key identifier used to encrypt the distributed image.
--
-- 'launchPermission', 'amiDistributionConfiguration_launchPermission' - Launch permissions can be used to configure which Amazon Web Services
-- accounts can use the AMI to launch instances.
--
-- 'name', 'amiDistributionConfiguration_name' - The name of the output AMI.
--
-- 'targetAccountIds', 'amiDistributionConfiguration_targetAccountIds' - The ID of an account to which you want to distribute an image.
newAmiDistributionConfiguration ::
  AmiDistributionConfiguration
newAmiDistributionConfiguration =
  AmiDistributionConfiguration'
    { amiTags =
        Prelude.Nothing,
      description = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      launchPermission = Prelude.Nothing,
      name = Prelude.Nothing,
      targetAccountIds = Prelude.Nothing
    }

-- | The tags to apply to AMIs distributed to this Region.
amiDistributionConfiguration_amiTags :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
amiDistributionConfiguration_amiTags = Lens.lens (\AmiDistributionConfiguration' {amiTags} -> amiTags) (\s@AmiDistributionConfiguration' {} a -> s {amiTags = a} :: AmiDistributionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The description of the AMI distribution configuration. Minimum and
-- maximum length are in characters.
amiDistributionConfiguration_description :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe Prelude.Text)
amiDistributionConfiguration_description = Lens.lens (\AmiDistributionConfiguration' {description} -> description) (\s@AmiDistributionConfiguration' {} a -> s {description = a} :: AmiDistributionConfiguration)

-- | The KMS key identifier used to encrypt the distributed image.
amiDistributionConfiguration_kmsKeyId :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe Prelude.Text)
amiDistributionConfiguration_kmsKeyId = Lens.lens (\AmiDistributionConfiguration' {kmsKeyId} -> kmsKeyId) (\s@AmiDistributionConfiguration' {} a -> s {kmsKeyId = a} :: AmiDistributionConfiguration)

-- | Launch permissions can be used to configure which Amazon Web Services
-- accounts can use the AMI to launch instances.
amiDistributionConfiguration_launchPermission :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe LaunchPermissionConfiguration)
amiDistributionConfiguration_launchPermission = Lens.lens (\AmiDistributionConfiguration' {launchPermission} -> launchPermission) (\s@AmiDistributionConfiguration' {} a -> s {launchPermission = a} :: AmiDistributionConfiguration)

-- | The name of the output AMI.
amiDistributionConfiguration_name :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe Prelude.Text)
amiDistributionConfiguration_name = Lens.lens (\AmiDistributionConfiguration' {name} -> name) (\s@AmiDistributionConfiguration' {} a -> s {name = a} :: AmiDistributionConfiguration)

-- | The ID of an account to which you want to distribute an image.
amiDistributionConfiguration_targetAccountIds :: Lens.Lens' AmiDistributionConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
amiDistributionConfiguration_targetAccountIds = Lens.lens (\AmiDistributionConfiguration' {targetAccountIds} -> targetAccountIds) (\s@AmiDistributionConfiguration' {} a -> s {targetAccountIds = a} :: AmiDistributionConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AmiDistributionConfiguration where
  parseJSON =
    Data.withObject
      "AmiDistributionConfiguration"
      ( \x ->
          AmiDistributionConfiguration'
            Prelude.<$> (x Data..:? "amiTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "launchPermission")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "targetAccountIds")
      )

instance
  Prelude.Hashable
    AmiDistributionConfiguration
  where
  hashWithSalt _salt AmiDistributionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` amiTags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` launchPermission
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` targetAccountIds

instance Prelude.NFData AmiDistributionConfiguration where
  rnf AmiDistributionConfiguration' {..} =
    Prelude.rnf amiTags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf launchPermission
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf targetAccountIds

instance Data.ToJSON AmiDistributionConfiguration where
  toJSON AmiDistributionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("amiTags" Data..=) Prelude.<$> amiTags,
            ("description" Data..=) Prelude.<$> description,
            ("kmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("launchPermission" Data..=)
              Prelude.<$> launchPermission,
            ("name" Data..=) Prelude.<$> name,
            ("targetAccountIds" Data..=)
              Prelude.<$> targetAccountIds
          ]
      )
