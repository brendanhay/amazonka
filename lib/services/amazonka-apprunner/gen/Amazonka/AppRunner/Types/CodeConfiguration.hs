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
-- Module      : Amazonka.AppRunner.Types.CodeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.CodeConfiguration where

import Amazonka.AppRunner.Types.CodeConfigurationValues
import Amazonka.AppRunner.Types.ConfigurationSource
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration that App Runner uses to build and run an App
-- Runner service from a source code repository.
--
-- /See:/ 'newCodeConfiguration' smart constructor.
data CodeConfiguration = CodeConfiguration'
  { -- | The basic configuration for building and running the App Runner service.
    -- Use it to quickly launch an App Runner service without providing a
    -- @apprunner.yaml@ file in the source code repository (or ignoring the
    -- file if it exists).
    codeConfigurationValues :: Prelude.Maybe CodeConfigurationValues,
    -- | The source of the App Runner configuration. Values are interpreted as
    -- follows:
    --
    -- -   @REPOSITORY@ – App Runner reads configuration values from the
    --     @apprunner.yaml@ file in the source code repository and ignores
    --     @CodeConfigurationValues@.
    --
    -- -   @API@ – App Runner uses configuration values provided in
    --     @CodeConfigurationValues@ and ignores the @apprunner.yaml@ file in
    --     the source code repository.
    configurationSource :: ConfigurationSource
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeConfigurationValues', 'codeConfiguration_codeConfigurationValues' - The basic configuration for building and running the App Runner service.
-- Use it to quickly launch an App Runner service without providing a
-- @apprunner.yaml@ file in the source code repository (or ignoring the
-- file if it exists).
--
-- 'configurationSource', 'codeConfiguration_configurationSource' - The source of the App Runner configuration. Values are interpreted as
-- follows:
--
-- -   @REPOSITORY@ – App Runner reads configuration values from the
--     @apprunner.yaml@ file in the source code repository and ignores
--     @CodeConfigurationValues@.
--
-- -   @API@ – App Runner uses configuration values provided in
--     @CodeConfigurationValues@ and ignores the @apprunner.yaml@ file in
--     the source code repository.
newCodeConfiguration ::
  -- | 'configurationSource'
  ConfigurationSource ->
  CodeConfiguration
newCodeConfiguration pConfigurationSource_ =
  CodeConfiguration'
    { codeConfigurationValues =
        Prelude.Nothing,
      configurationSource = pConfigurationSource_
    }

-- | The basic configuration for building and running the App Runner service.
-- Use it to quickly launch an App Runner service without providing a
-- @apprunner.yaml@ file in the source code repository (or ignoring the
-- file if it exists).
codeConfiguration_codeConfigurationValues :: Lens.Lens' CodeConfiguration (Prelude.Maybe CodeConfigurationValues)
codeConfiguration_codeConfigurationValues = Lens.lens (\CodeConfiguration' {codeConfigurationValues} -> codeConfigurationValues) (\s@CodeConfiguration' {} a -> s {codeConfigurationValues = a} :: CodeConfiguration)

-- | The source of the App Runner configuration. Values are interpreted as
-- follows:
--
-- -   @REPOSITORY@ – App Runner reads configuration values from the
--     @apprunner.yaml@ file in the source code repository and ignores
--     @CodeConfigurationValues@.
--
-- -   @API@ – App Runner uses configuration values provided in
--     @CodeConfigurationValues@ and ignores the @apprunner.yaml@ file in
--     the source code repository.
codeConfiguration_configurationSource :: Lens.Lens' CodeConfiguration ConfigurationSource
codeConfiguration_configurationSource = Lens.lens (\CodeConfiguration' {configurationSource} -> configurationSource) (\s@CodeConfiguration' {} a -> s {configurationSource = a} :: CodeConfiguration)

instance Data.FromJSON CodeConfiguration where
  parseJSON =
    Data.withObject
      "CodeConfiguration"
      ( \x ->
          CodeConfiguration'
            Prelude.<$> (x Data..:? "CodeConfigurationValues")
            Prelude.<*> (x Data..: "ConfigurationSource")
      )

instance Prelude.Hashable CodeConfiguration where
  hashWithSalt _salt CodeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` codeConfigurationValues
      `Prelude.hashWithSalt` configurationSource

instance Prelude.NFData CodeConfiguration where
  rnf CodeConfiguration' {..} =
    Prelude.rnf codeConfigurationValues `Prelude.seq`
      Prelude.rnf configurationSource

instance Data.ToJSON CodeConfiguration where
  toJSON CodeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CodeConfigurationValues" Data..=)
              Prelude.<$> codeConfigurationValues,
            Prelude.Just
              ("ConfigurationSource" Data..= configurationSource)
          ]
      )
