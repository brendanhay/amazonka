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
-- Module      : Amazonka.AppConfig.Types.ConfigurationProfileSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.ConfigurationProfileSummary where

import Amazonka.AppConfig.Types.ValidatorType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of a configuration profile.
--
-- /See:/ 'newConfigurationProfileSummary' smart constructor.
data ConfigurationProfileSummary = ConfigurationProfileSummary'
  { -- | The application ID.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the configuration profile.
    id :: Prelude.Maybe Prelude.Text,
    -- | The URI location of the configuration.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The name of the configuration profile.
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
    -- | The types of validators in the configuration profile.
    validatorTypes :: Prelude.Maybe [ValidatorType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationProfileSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'configurationProfileSummary_applicationId' - The application ID.
--
-- 'id', 'configurationProfileSummary_id' - The ID of the configuration profile.
--
-- 'locationUri', 'configurationProfileSummary_locationUri' - The URI location of the configuration.
--
-- 'name', 'configurationProfileSummary_name' - The name of the configuration profile.
--
-- 'type'', 'configurationProfileSummary_type' - The type of configurations contained in the profile. AppConfig supports
-- @feature flags@ and @freeform@ configurations. We recommend you create
-- feature flag configurations to enable or disable new features and
-- freeform configurations to distribute configurations to an application.
-- When calling this API, enter one of the following values for @Type@:
--
-- @AWS.AppConfig.FeatureFlags@
--
-- @AWS.Freeform@
--
-- 'validatorTypes', 'configurationProfileSummary_validatorTypes' - The types of validators in the configuration profile.
newConfigurationProfileSummary ::
  ConfigurationProfileSummary
newConfigurationProfileSummary =
  ConfigurationProfileSummary'
    { applicationId =
        Prelude.Nothing,
      id = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      validatorTypes = Prelude.Nothing
    }

-- | The application ID.
configurationProfileSummary_applicationId :: Lens.Lens' ConfigurationProfileSummary (Prelude.Maybe Prelude.Text)
configurationProfileSummary_applicationId = Lens.lens (\ConfigurationProfileSummary' {applicationId} -> applicationId) (\s@ConfigurationProfileSummary' {} a -> s {applicationId = a} :: ConfigurationProfileSummary)

-- | The ID of the configuration profile.
configurationProfileSummary_id :: Lens.Lens' ConfigurationProfileSummary (Prelude.Maybe Prelude.Text)
configurationProfileSummary_id = Lens.lens (\ConfigurationProfileSummary' {id} -> id) (\s@ConfigurationProfileSummary' {} a -> s {id = a} :: ConfigurationProfileSummary)

-- | The URI location of the configuration.
configurationProfileSummary_locationUri :: Lens.Lens' ConfigurationProfileSummary (Prelude.Maybe Prelude.Text)
configurationProfileSummary_locationUri = Lens.lens (\ConfigurationProfileSummary' {locationUri} -> locationUri) (\s@ConfigurationProfileSummary' {} a -> s {locationUri = a} :: ConfigurationProfileSummary)

-- | The name of the configuration profile.
configurationProfileSummary_name :: Lens.Lens' ConfigurationProfileSummary (Prelude.Maybe Prelude.Text)
configurationProfileSummary_name = Lens.lens (\ConfigurationProfileSummary' {name} -> name) (\s@ConfigurationProfileSummary' {} a -> s {name = a} :: ConfigurationProfileSummary)

-- | The type of configurations contained in the profile. AppConfig supports
-- @feature flags@ and @freeform@ configurations. We recommend you create
-- feature flag configurations to enable or disable new features and
-- freeform configurations to distribute configurations to an application.
-- When calling this API, enter one of the following values for @Type@:
--
-- @AWS.AppConfig.FeatureFlags@
--
-- @AWS.Freeform@
configurationProfileSummary_type :: Lens.Lens' ConfigurationProfileSummary (Prelude.Maybe Prelude.Text)
configurationProfileSummary_type = Lens.lens (\ConfigurationProfileSummary' {type'} -> type') (\s@ConfigurationProfileSummary' {} a -> s {type' = a} :: ConfigurationProfileSummary)

-- | The types of validators in the configuration profile.
configurationProfileSummary_validatorTypes :: Lens.Lens' ConfigurationProfileSummary (Prelude.Maybe [ValidatorType])
configurationProfileSummary_validatorTypes = Lens.lens (\ConfigurationProfileSummary' {validatorTypes} -> validatorTypes) (\s@ConfigurationProfileSummary' {} a -> s {validatorTypes = a} :: ConfigurationProfileSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ConfigurationProfileSummary where
  parseJSON =
    Data.withObject
      "ConfigurationProfileSummary"
      ( \x ->
          ConfigurationProfileSummary'
            Prelude.<$> (x Data..:? "ApplicationId")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LocationUri")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> ( x Data..:? "ValidatorTypes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ConfigurationProfileSummary where
  hashWithSalt _salt ConfigurationProfileSummary' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` locationUri
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` validatorTypes

instance Prelude.NFData ConfigurationProfileSummary where
  rnf ConfigurationProfileSummary' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf validatorTypes
