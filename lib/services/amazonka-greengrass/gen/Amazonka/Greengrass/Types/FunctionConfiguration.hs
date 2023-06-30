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
-- Module      : Amazonka.Greengrass.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.FunctionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.EncodingType
import Amazonka.Greengrass.Types.FunctionConfigurationEnvironment
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the Lambda function.
--
-- /See:/ 'newFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The expected encoding type of the input payload for the function. The
    -- default is \'\'json\'\'.
    encodingType :: Prelude.Maybe EncodingType,
    -- | The environment configuration of the function.
    environment :: Prelude.Maybe FunctionConfigurationEnvironment,
    -- | The execution arguments.
    execArgs :: Prelude.Maybe Prelude.Text,
    -- | The name of the function executable.
    executable :: Prelude.Maybe Prelude.Text,
    -- | The Lambda runtime supported by Greengrass which is to be used instead
    -- of the one specified in the Lambda function.
    functionRuntimeOverride :: Prelude.Maybe Prelude.Text,
    -- | The memory size, in KB, which the function requires. This setting is not
    -- applicable and should be cleared when you run the Lambda function
    -- without containerization.
    memorySize :: Prelude.Maybe Prelude.Int,
    -- | True if the function is pinned. Pinned means the function is long-lived
    -- and starts when the core starts.
    pinned :: Prelude.Maybe Prelude.Bool,
    -- | The allowed function execution time, after which Lambda should terminate
    -- the function. This timeout still applies to pinned Lambda functions for
    -- each request.
    timeout :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encodingType', 'functionConfiguration_encodingType' - The expected encoding type of the input payload for the function. The
-- default is \'\'json\'\'.
--
-- 'environment', 'functionConfiguration_environment' - The environment configuration of the function.
--
-- 'execArgs', 'functionConfiguration_execArgs' - The execution arguments.
--
-- 'executable', 'functionConfiguration_executable' - The name of the function executable.
--
-- 'functionRuntimeOverride', 'functionConfiguration_functionRuntimeOverride' - The Lambda runtime supported by Greengrass which is to be used instead
-- of the one specified in the Lambda function.
--
-- 'memorySize', 'functionConfiguration_memorySize' - The memory size, in KB, which the function requires. This setting is not
-- applicable and should be cleared when you run the Lambda function
-- without containerization.
--
-- 'pinned', 'functionConfiguration_pinned' - True if the function is pinned. Pinned means the function is long-lived
-- and starts when the core starts.
--
-- 'timeout', 'functionConfiguration_timeout' - The allowed function execution time, after which Lambda should terminate
-- the function. This timeout still applies to pinned Lambda functions for
-- each request.
newFunctionConfiguration ::
  FunctionConfiguration
newFunctionConfiguration =
  FunctionConfiguration'
    { encodingType =
        Prelude.Nothing,
      environment = Prelude.Nothing,
      execArgs = Prelude.Nothing,
      executable = Prelude.Nothing,
      functionRuntimeOverride = Prelude.Nothing,
      memorySize = Prelude.Nothing,
      pinned = Prelude.Nothing,
      timeout = Prelude.Nothing
    }

-- | The expected encoding type of the input payload for the function. The
-- default is \'\'json\'\'.
functionConfiguration_encodingType :: Lens.Lens' FunctionConfiguration (Prelude.Maybe EncodingType)
functionConfiguration_encodingType = Lens.lens (\FunctionConfiguration' {encodingType} -> encodingType) (\s@FunctionConfiguration' {} a -> s {encodingType = a} :: FunctionConfiguration)

-- | The environment configuration of the function.
functionConfiguration_environment :: Lens.Lens' FunctionConfiguration (Prelude.Maybe FunctionConfigurationEnvironment)
functionConfiguration_environment = Lens.lens (\FunctionConfiguration' {environment} -> environment) (\s@FunctionConfiguration' {} a -> s {environment = a} :: FunctionConfiguration)

-- | The execution arguments.
functionConfiguration_execArgs :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_execArgs = Lens.lens (\FunctionConfiguration' {execArgs} -> execArgs) (\s@FunctionConfiguration' {} a -> s {execArgs = a} :: FunctionConfiguration)

-- | The name of the function executable.
functionConfiguration_executable :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_executable = Lens.lens (\FunctionConfiguration' {executable} -> executable) (\s@FunctionConfiguration' {} a -> s {executable = a} :: FunctionConfiguration)

-- | The Lambda runtime supported by Greengrass which is to be used instead
-- of the one specified in the Lambda function.
functionConfiguration_functionRuntimeOverride :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_functionRuntimeOverride = Lens.lens (\FunctionConfiguration' {functionRuntimeOverride} -> functionRuntimeOverride) (\s@FunctionConfiguration' {} a -> s {functionRuntimeOverride = a} :: FunctionConfiguration)

-- | The memory size, in KB, which the function requires. This setting is not
-- applicable and should be cleared when you run the Lambda function
-- without containerization.
functionConfiguration_memorySize :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Int)
functionConfiguration_memorySize = Lens.lens (\FunctionConfiguration' {memorySize} -> memorySize) (\s@FunctionConfiguration' {} a -> s {memorySize = a} :: FunctionConfiguration)

-- | True if the function is pinned. Pinned means the function is long-lived
-- and starts when the core starts.
functionConfiguration_pinned :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Bool)
functionConfiguration_pinned = Lens.lens (\FunctionConfiguration' {pinned} -> pinned) (\s@FunctionConfiguration' {} a -> s {pinned = a} :: FunctionConfiguration)

-- | The allowed function execution time, after which Lambda should terminate
-- the function. This timeout still applies to pinned Lambda functions for
-- each request.
functionConfiguration_timeout :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Int)
functionConfiguration_timeout = Lens.lens (\FunctionConfiguration' {timeout} -> timeout) (\s@FunctionConfiguration' {} a -> s {timeout = a} :: FunctionConfiguration)

instance Data.FromJSON FunctionConfiguration where
  parseJSON =
    Data.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Prelude.<$> (x Data..:? "EncodingType")
            Prelude.<*> (x Data..:? "Environment")
            Prelude.<*> (x Data..:? "ExecArgs")
            Prelude.<*> (x Data..:? "Executable")
            Prelude.<*> (x Data..:? "FunctionRuntimeOverride")
            Prelude.<*> (x Data..:? "MemorySize")
            Prelude.<*> (x Data..:? "Pinned")
            Prelude.<*> (x Data..:? "Timeout")
      )

instance Prelude.Hashable FunctionConfiguration where
  hashWithSalt _salt FunctionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` encodingType
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` execArgs
      `Prelude.hashWithSalt` executable
      `Prelude.hashWithSalt` functionRuntimeOverride
      `Prelude.hashWithSalt` memorySize
      `Prelude.hashWithSalt` pinned
      `Prelude.hashWithSalt` timeout

instance Prelude.NFData FunctionConfiguration where
  rnf FunctionConfiguration' {..} =
    Prelude.rnf encodingType
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf execArgs
      `Prelude.seq` Prelude.rnf executable
      `Prelude.seq` Prelude.rnf functionRuntimeOverride
      `Prelude.seq` Prelude.rnf memorySize
      `Prelude.seq` Prelude.rnf pinned
      `Prelude.seq` Prelude.rnf timeout

instance Data.ToJSON FunctionConfiguration where
  toJSON FunctionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncodingType" Data..=) Prelude.<$> encodingType,
            ("Environment" Data..=) Prelude.<$> environment,
            ("ExecArgs" Data..=) Prelude.<$> execArgs,
            ("Executable" Data..=) Prelude.<$> executable,
            ("FunctionRuntimeOverride" Data..=)
              Prelude.<$> functionRuntimeOverride,
            ("MemorySize" Data..=) Prelude.<$> memorySize,
            ("Pinned" Data..=) Prelude.<$> pinned,
            ("Timeout" Data..=) Prelude.<$> timeout
          ]
      )
