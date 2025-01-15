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
-- Module      : Amazonka.Greengrass.Types.FunctionDefinitionVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.FunctionDefinitionVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.Function
import Amazonka.Greengrass.Types.FunctionDefaultConfig
import qualified Amazonka.Prelude as Prelude

-- | Information about a function definition version.
--
-- /See:/ 'newFunctionDefinitionVersion' smart constructor.
data FunctionDefinitionVersion = FunctionDefinitionVersion'
  { -- | The default configuration that applies to all Lambda functions in this
    -- function definition version. Individual Lambda functions can override
    -- these settings.
    defaultConfig :: Prelude.Maybe FunctionDefaultConfig,
    -- | A list of Lambda functions in this function definition version.
    functions :: Prelude.Maybe [Function]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionDefinitionVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultConfig', 'functionDefinitionVersion_defaultConfig' - The default configuration that applies to all Lambda functions in this
-- function definition version. Individual Lambda functions can override
-- these settings.
--
-- 'functions', 'functionDefinitionVersion_functions' - A list of Lambda functions in this function definition version.
newFunctionDefinitionVersion ::
  FunctionDefinitionVersion
newFunctionDefinitionVersion =
  FunctionDefinitionVersion'
    { defaultConfig =
        Prelude.Nothing,
      functions = Prelude.Nothing
    }

-- | The default configuration that applies to all Lambda functions in this
-- function definition version. Individual Lambda functions can override
-- these settings.
functionDefinitionVersion_defaultConfig :: Lens.Lens' FunctionDefinitionVersion (Prelude.Maybe FunctionDefaultConfig)
functionDefinitionVersion_defaultConfig = Lens.lens (\FunctionDefinitionVersion' {defaultConfig} -> defaultConfig) (\s@FunctionDefinitionVersion' {} a -> s {defaultConfig = a} :: FunctionDefinitionVersion)

-- | A list of Lambda functions in this function definition version.
functionDefinitionVersion_functions :: Lens.Lens' FunctionDefinitionVersion (Prelude.Maybe [Function])
functionDefinitionVersion_functions = Lens.lens (\FunctionDefinitionVersion' {functions} -> functions) (\s@FunctionDefinitionVersion' {} a -> s {functions = a} :: FunctionDefinitionVersion) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FunctionDefinitionVersion where
  parseJSON =
    Data.withObject
      "FunctionDefinitionVersion"
      ( \x ->
          FunctionDefinitionVersion'
            Prelude.<$> (x Data..:? "DefaultConfig")
            Prelude.<*> (x Data..:? "Functions" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FunctionDefinitionVersion where
  hashWithSalt _salt FunctionDefinitionVersion' {..} =
    _salt
      `Prelude.hashWithSalt` defaultConfig
      `Prelude.hashWithSalt` functions

instance Prelude.NFData FunctionDefinitionVersion where
  rnf FunctionDefinitionVersion' {..} =
    Prelude.rnf defaultConfig `Prelude.seq`
      Prelude.rnf functions

instance Data.ToJSON FunctionDefinitionVersion where
  toJSON FunctionDefinitionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultConfig" Data..=) Prelude.<$> defaultConfig,
            ("Functions" Data..=) Prelude.<$> functions
          ]
      )
