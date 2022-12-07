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
-- Module      : Amazonka.SMS.Types.AppValidationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.AppValidationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SMS.Types.AppValidationStrategy
import Amazonka.SMS.Types.SSMValidationParameters

-- | Configuration for validating an application.
--
-- /See:/ 'newAppValidationConfiguration' smart constructor.
data AppValidationConfiguration = AppValidationConfiguration'
  { -- | The name of the configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The validation parameters.
    ssmValidationParameters :: Prelude.Maybe SSMValidationParameters,
    -- | The validation strategy.
    appValidationStrategy :: Prelude.Maybe AppValidationStrategy,
    -- | The ID of the validation.
    validationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'appValidationConfiguration_name' - The name of the configuration.
--
-- 'ssmValidationParameters', 'appValidationConfiguration_ssmValidationParameters' - The validation parameters.
--
-- 'appValidationStrategy', 'appValidationConfiguration_appValidationStrategy' - The validation strategy.
--
-- 'validationId', 'appValidationConfiguration_validationId' - The ID of the validation.
newAppValidationConfiguration ::
  AppValidationConfiguration
newAppValidationConfiguration =
  AppValidationConfiguration'
    { name = Prelude.Nothing,
      ssmValidationParameters = Prelude.Nothing,
      appValidationStrategy = Prelude.Nothing,
      validationId = Prelude.Nothing
    }

-- | The name of the configuration.
appValidationConfiguration_name :: Lens.Lens' AppValidationConfiguration (Prelude.Maybe Prelude.Text)
appValidationConfiguration_name = Lens.lens (\AppValidationConfiguration' {name} -> name) (\s@AppValidationConfiguration' {} a -> s {name = a} :: AppValidationConfiguration)

-- | The validation parameters.
appValidationConfiguration_ssmValidationParameters :: Lens.Lens' AppValidationConfiguration (Prelude.Maybe SSMValidationParameters)
appValidationConfiguration_ssmValidationParameters = Lens.lens (\AppValidationConfiguration' {ssmValidationParameters} -> ssmValidationParameters) (\s@AppValidationConfiguration' {} a -> s {ssmValidationParameters = a} :: AppValidationConfiguration)

-- | The validation strategy.
appValidationConfiguration_appValidationStrategy :: Lens.Lens' AppValidationConfiguration (Prelude.Maybe AppValidationStrategy)
appValidationConfiguration_appValidationStrategy = Lens.lens (\AppValidationConfiguration' {appValidationStrategy} -> appValidationStrategy) (\s@AppValidationConfiguration' {} a -> s {appValidationStrategy = a} :: AppValidationConfiguration)

-- | The ID of the validation.
appValidationConfiguration_validationId :: Lens.Lens' AppValidationConfiguration (Prelude.Maybe Prelude.Text)
appValidationConfiguration_validationId = Lens.lens (\AppValidationConfiguration' {validationId} -> validationId) (\s@AppValidationConfiguration' {} a -> s {validationId = a} :: AppValidationConfiguration)

instance Data.FromJSON AppValidationConfiguration where
  parseJSON =
    Data.withObject
      "AppValidationConfiguration"
      ( \x ->
          AppValidationConfiguration'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "ssmValidationParameters")
            Prelude.<*> (x Data..:? "appValidationStrategy")
            Prelude.<*> (x Data..:? "validationId")
      )

instance Prelude.Hashable AppValidationConfiguration where
  hashWithSalt _salt AppValidationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ssmValidationParameters
      `Prelude.hashWithSalt` appValidationStrategy
      `Prelude.hashWithSalt` validationId

instance Prelude.NFData AppValidationConfiguration where
  rnf AppValidationConfiguration' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf ssmValidationParameters
      `Prelude.seq` Prelude.rnf appValidationStrategy
      `Prelude.seq` Prelude.rnf validationId

instance Data.ToJSON AppValidationConfiguration where
  toJSON AppValidationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("ssmValidationParameters" Data..=)
              Prelude.<$> ssmValidationParameters,
            ("appValidationStrategy" Data..=)
              Prelude.<$> appValidationStrategy,
            ("validationId" Data..=) Prelude.<$> validationId
          ]
      )
