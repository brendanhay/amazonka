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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | A list of Lambda functions in this function definition version.
    functions :: Prelude.Maybe [Function],
    -- | The default configuration that applies to all Lambda functions in this
    -- function definition version. Individual Lambda functions can override
    -- these settings.
    defaultConfig :: Prelude.Maybe FunctionDefaultConfig
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
-- 'functions', 'functionDefinitionVersion_functions' - A list of Lambda functions in this function definition version.
--
-- 'defaultConfig', 'functionDefinitionVersion_defaultConfig' - The default configuration that applies to all Lambda functions in this
-- function definition version. Individual Lambda functions can override
-- these settings.
newFunctionDefinitionVersion ::
  FunctionDefinitionVersion
newFunctionDefinitionVersion =
  FunctionDefinitionVersion'
    { functions =
        Prelude.Nothing,
      defaultConfig = Prelude.Nothing
    }

-- | A list of Lambda functions in this function definition version.
functionDefinitionVersion_functions :: Lens.Lens' FunctionDefinitionVersion (Prelude.Maybe [Function])
functionDefinitionVersion_functions = Lens.lens (\FunctionDefinitionVersion' {functions} -> functions) (\s@FunctionDefinitionVersion' {} a -> s {functions = a} :: FunctionDefinitionVersion) Prelude.. Lens.mapping Lens.coerced

-- | The default configuration that applies to all Lambda functions in this
-- function definition version. Individual Lambda functions can override
-- these settings.
functionDefinitionVersion_defaultConfig :: Lens.Lens' FunctionDefinitionVersion (Prelude.Maybe FunctionDefaultConfig)
functionDefinitionVersion_defaultConfig = Lens.lens (\FunctionDefinitionVersion' {defaultConfig} -> defaultConfig) (\s@FunctionDefinitionVersion' {} a -> s {defaultConfig = a} :: FunctionDefinitionVersion)

instance Data.FromJSON FunctionDefinitionVersion where
  parseJSON =
    Data.withObject
      "FunctionDefinitionVersion"
      ( \x ->
          FunctionDefinitionVersion'
            Prelude.<$> (x Data..:? "Functions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "DefaultConfig")
      )

instance Prelude.Hashable FunctionDefinitionVersion where
  hashWithSalt _salt FunctionDefinitionVersion' {..} =
    _salt `Prelude.hashWithSalt` functions
      `Prelude.hashWithSalt` defaultConfig

instance Prelude.NFData FunctionDefinitionVersion where
  rnf FunctionDefinitionVersion' {..} =
    Prelude.rnf functions
      `Prelude.seq` Prelude.rnf defaultConfig

instance Data.ToJSON FunctionDefinitionVersion where
  toJSON FunctionDefinitionVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Functions" Data..=) Prelude.<$> functions,
            ("DefaultConfig" Data..=) Prelude.<$> defaultConfig
          ]
      )
