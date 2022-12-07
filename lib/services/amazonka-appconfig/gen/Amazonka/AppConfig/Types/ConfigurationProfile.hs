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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.ConfigurationProfile where

import Amazonka.AppConfig.Types.Validator
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newConfigurationProfile' smart constructor.
data ConfigurationProfile = ConfigurationProfile'
  { -- | The name of the configuration profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of configurations contained in the profile. AppConfig supports
    -- @feature flags@ and @freeform@ configurations. We recommend you create
    -- feature flag configurations to enable or disable new features and
    -- freeform configurations to distribute configurations to an application.
    -- When calling this API, enter one of the following values for @Type@:
    --
    -- @AWS.AppConfig.FeatureFlags@
    --
    -- @AWS.Freeform@
    type' :: Prelude.Maybe Prelude.Text,
    -- | The ARN of an IAM role with permission to access the configuration at
    -- the specified @LocationUri@.
    retrievalRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The configuration profile ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The configuration profile description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The URI location of the configuration.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The application ID.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | A list of methods for validating the configuration.
    validators :: Prelude.Maybe [Validator]
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
-- 'name', 'configurationProfile_name' - The name of the configuration profile.
--
-- 'type'', 'configurationProfile_type' - The type of configurations contained in the profile. AppConfig supports
-- @feature flags@ and @freeform@ configurations. We recommend you create
-- feature flag configurations to enable or disable new features and
-- freeform configurations to distribute configurations to an application.
-- When calling this API, enter one of the following values for @Type@:
--
-- @AWS.AppConfig.FeatureFlags@
--
-- @AWS.Freeform@
--
-- 'retrievalRoleArn', 'configurationProfile_retrievalRoleArn' - The ARN of an IAM role with permission to access the configuration at
-- the specified @LocationUri@.
--
-- 'id', 'configurationProfile_id' - The configuration profile ID.
--
-- 'description', 'configurationProfile_description' - The configuration profile description.
--
-- 'locationUri', 'configurationProfile_locationUri' - The URI location of the configuration.
--
-- 'applicationId', 'configurationProfile_applicationId' - The application ID.
--
-- 'validators', 'configurationProfile_validators' - A list of methods for validating the configuration.
newConfigurationProfile ::
  ConfigurationProfile
newConfigurationProfile =
  ConfigurationProfile'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      retrievalRoleArn = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      validators = Prelude.Nothing
    }

-- | The name of the configuration profile.
configurationProfile_name :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_name = Lens.lens (\ConfigurationProfile' {name} -> name) (\s@ConfigurationProfile' {} a -> s {name = a} :: ConfigurationProfile)

-- | The type of configurations contained in the profile. AppConfig supports
-- @feature flags@ and @freeform@ configurations. We recommend you create
-- feature flag configurations to enable or disable new features and
-- freeform configurations to distribute configurations to an application.
-- When calling this API, enter one of the following values for @Type@:
--
-- @AWS.AppConfig.FeatureFlags@
--
-- @AWS.Freeform@
configurationProfile_type :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_type = Lens.lens (\ConfigurationProfile' {type'} -> type') (\s@ConfigurationProfile' {} a -> s {type' = a} :: ConfigurationProfile)

-- | The ARN of an IAM role with permission to access the configuration at
-- the specified @LocationUri@.
configurationProfile_retrievalRoleArn :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_retrievalRoleArn = Lens.lens (\ConfigurationProfile' {retrievalRoleArn} -> retrievalRoleArn) (\s@ConfigurationProfile' {} a -> s {retrievalRoleArn = a} :: ConfigurationProfile)

-- | The configuration profile ID.
configurationProfile_id :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_id = Lens.lens (\ConfigurationProfile' {id} -> id) (\s@ConfigurationProfile' {} a -> s {id = a} :: ConfigurationProfile)

-- | The configuration profile description.
configurationProfile_description :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_description = Lens.lens (\ConfigurationProfile' {description} -> description) (\s@ConfigurationProfile' {} a -> s {description = a} :: ConfigurationProfile)

-- | The URI location of the configuration.
configurationProfile_locationUri :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_locationUri = Lens.lens (\ConfigurationProfile' {locationUri} -> locationUri) (\s@ConfigurationProfile' {} a -> s {locationUri = a} :: ConfigurationProfile)

-- | The application ID.
configurationProfile_applicationId :: Lens.Lens' ConfigurationProfile (Prelude.Maybe Prelude.Text)
configurationProfile_applicationId = Lens.lens (\ConfigurationProfile' {applicationId} -> applicationId) (\s@ConfigurationProfile' {} a -> s {applicationId = a} :: ConfigurationProfile)

-- | A list of methods for validating the configuration.
configurationProfile_validators :: Lens.Lens' ConfigurationProfile (Prelude.Maybe [Validator])
configurationProfile_validators = Lens.lens (\ConfigurationProfile' {validators} -> validators) (\s@ConfigurationProfile' {} a -> s {validators = a} :: ConfigurationProfile) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ConfigurationProfile where
  parseJSON =
    Data.withObject
      "ConfigurationProfile"
      ( \x ->
          ConfigurationProfile'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "RetrievalRoleArn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LocationUri")
            Prelude.<*> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "Validators" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ConfigurationProfile where
  hashWithSalt _salt ConfigurationProfile' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` retrievalRoleArn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` locationUri
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` validators

instance Prelude.NFData ConfigurationProfile where
  rnf ConfigurationProfile' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf retrievalRoleArn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf validators
