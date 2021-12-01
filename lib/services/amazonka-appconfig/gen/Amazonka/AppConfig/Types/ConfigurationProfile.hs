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
-- Module      : Amazonka.AppConfig.Types.ConfigurationProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.ConfigurationProfile where

import Amazonka.AppConfig.Types.Validator
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newConfigurationProfile' smart constructor.
data ConfigurationProfile = ConfigurationProfile'
  { -- | The ARN of an IAM role with permission to access the configuration at
    -- the specified LocationUri.
    retrievalRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of methods for validating the configuration.
    validators :: Prelude.Maybe [Validator],
    -- | The URI location of the configuration.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The application ID.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The configuration profile ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The configuration profile description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retrievalRoleArn', 'configurationProfile_retrievalRoleArn' - The ARN of an IAM role with permission to access the configuration at
-- the specified LocationUri.
--
-- 'validators', 'configurationProfile_validators' - A list of methods for validating the configuration.
--
-- 'locationUri', 'configurationProfile_locationUri' - The URI location of the configuration.
--
-- 'applicationId', 'configurationProfile_applicationId' - The application ID.
--
-- 'name', 'configurationProfile_name' - The name of the configuration profile.
--
-- 'id', 'configurationProfile_id' - The configuration profile ID.
--
-- 'description', 'configurationProfile_description' - The configuration profile description.
newConfigurationProfile ::
  ConfigurationProfile
newConfigurationProfile =
  ConfigurationProfile'
    { retrievalRoleArn =
        Prelude.Nothing,
      validators = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ARN of an IAM role with permission to access the configuration at
-- the specified LocationUri.
configurationProfile_retrievalRoleArn :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_retrievalRoleArn = Lens.lens (\ConfigurationProfile' {retrievalRoleArn} -> retrievalRoleArn) (\s@ConfigurationProfile' {} a -> s {retrievalRoleArn = a} :: ConfigurationProfile)

-- | A list of methods for validating the configuration.
configurationProfile_validators :: Lens.Lens' ConfigurationProfile (Prelude.Maybe [Validator])
configurationProfile_validators = Lens.lens (\ConfigurationProfile' {validators} -> validators) (\s@ConfigurationProfile' {} a -> s {validators = a} :: ConfigurationProfile) Prelude.. Lens.mapping Lens.coerced

-- | The URI location of the configuration.
configurationProfile_locationUri :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_locationUri = Lens.lens (\ConfigurationProfile' {locationUri} -> locationUri) (\s@ConfigurationProfile' {} a -> s {locationUri = a} :: ConfigurationProfile)

-- | The application ID.
configurationProfile_applicationId :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_applicationId = Lens.lens (\ConfigurationProfile' {applicationId} -> applicationId) (\s@ConfigurationProfile' {} a -> s {applicationId = a} :: ConfigurationProfile)

-- | The name of the configuration profile.
configurationProfile_name :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_name = Lens.lens (\ConfigurationProfile' {name} -> name) (\s@ConfigurationProfile' {} a -> s {name = a} :: ConfigurationProfile)

-- | The configuration profile ID.
configurationProfile_id :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_id = Lens.lens (\ConfigurationProfile' {id} -> id) (\s@ConfigurationProfile' {} a -> s {id = a} :: ConfigurationProfile)

-- | The configuration profile description.
configurationProfile_description :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_description = Lens.lens (\ConfigurationProfile' {description} -> description) (\s@ConfigurationProfile' {} a -> s {description = a} :: ConfigurationProfile)

instance Core.FromJSON ConfigurationProfile where
  parseJSON =
    Core.withObject
      "ConfigurationProfile"
      ( \x ->
          ConfigurationProfile'
            Prelude.<$> (x Core..:? "RetrievalRoleArn")
            Prelude.<*> (x Core..:? "Validators" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "LocationUri")
            Prelude.<*> (x Core..:? "ApplicationId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable ConfigurationProfile where
  hashWithSalt salt' ConfigurationProfile' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` locationUri
      `Prelude.hashWithSalt` validators
      `Prelude.hashWithSalt` retrievalRoleArn

instance Prelude.NFData ConfigurationProfile where
  rnf ConfigurationProfile' {..} =
    Prelude.rnf retrievalRoleArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf validators
