{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Greengrass.Types.FunctionDefinitionVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionDefinitionVersion where

import Network.AWS.Greengrass.Types.Function
import Network.AWS.Greengrass.Types.FunctionDefaultConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
functionDefinitionVersion_functions = Lens.lens (\FunctionDefinitionVersion' {functions} -> functions) (\s@FunctionDefinitionVersion' {} a -> s {functions = a} :: FunctionDefinitionVersion) Prelude.. Lens.mapping Prelude._Coerce

-- | The default configuration that applies to all Lambda functions in this
-- function definition version. Individual Lambda functions can override
-- these settings.
functionDefinitionVersion_defaultConfig :: Lens.Lens' FunctionDefinitionVersion (Prelude.Maybe FunctionDefaultConfig)
functionDefinitionVersion_defaultConfig = Lens.lens (\FunctionDefinitionVersion' {defaultConfig} -> defaultConfig) (\s@FunctionDefinitionVersion' {} a -> s {defaultConfig = a} :: FunctionDefinitionVersion)

instance Prelude.FromJSON FunctionDefinitionVersion where
  parseJSON =
    Prelude.withObject
      "FunctionDefinitionVersion"
      ( \x ->
          FunctionDefinitionVersion'
            Prelude.<$> ( x Prelude..:? "Functions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "DefaultConfig")
      )

instance Prelude.Hashable FunctionDefinitionVersion

instance Prelude.NFData FunctionDefinitionVersion

instance Prelude.ToJSON FunctionDefinitionVersion where
  toJSON FunctionDefinitionVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Functions" Prelude..=) Prelude.<$> functions,
            ("DefaultConfig" Prelude..=)
              Prelude.<$> defaultConfig
          ]
      )
