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
-- Module      : Network.AWS.Greengrass.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionConfiguration where

import Network.AWS.Greengrass.Types.EncodingType
import Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The configuration of the Lambda function.
--
-- /See:/ 'newFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The execution arguments.
    execArgs :: Prelude.Maybe Prelude.Text,
    -- | The memory size, in KB, which the function requires. This setting is not
    -- applicable and should be cleared when you run the Lambda function
    -- without containerization.
    memorySize :: Prelude.Maybe Prelude.Int,
    -- | The allowed function execution time, after which Lambda should terminate
    -- the function. This timeout still applies to pinned Lambda functions for
    -- each request.
    timeout :: Prelude.Maybe Prelude.Int,
    -- | The expected encoding type of the input payload for the function. The
    -- default is \'\'json\'\'.
    encodingType :: Prelude.Maybe EncodingType,
    -- | True if the function is pinned. Pinned means the function is long-lived
    -- and starts when the core starts.
    pinned :: Prelude.Maybe Prelude.Bool,
    -- | The name of the function executable.
    executable :: Prelude.Maybe Prelude.Text,
    -- | The environment configuration of the function.
    environment :: Prelude.Maybe FunctionConfigurationEnvironment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FunctionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'execArgs', 'functionConfiguration_execArgs' - The execution arguments.
--
-- 'memorySize', 'functionConfiguration_memorySize' - The memory size, in KB, which the function requires. This setting is not
-- applicable and should be cleared when you run the Lambda function
-- without containerization.
--
-- 'timeout', 'functionConfiguration_timeout' - The allowed function execution time, after which Lambda should terminate
-- the function. This timeout still applies to pinned Lambda functions for
-- each request.
--
-- 'encodingType', 'functionConfiguration_encodingType' - The expected encoding type of the input payload for the function. The
-- default is \'\'json\'\'.
--
-- 'pinned', 'functionConfiguration_pinned' - True if the function is pinned. Pinned means the function is long-lived
-- and starts when the core starts.
--
-- 'executable', 'functionConfiguration_executable' - The name of the function executable.
--
-- 'environment', 'functionConfiguration_environment' - The environment configuration of the function.
newFunctionConfiguration ::
  FunctionConfiguration
newFunctionConfiguration =
  FunctionConfiguration'
    { execArgs = Prelude.Nothing,
      memorySize = Prelude.Nothing,
      timeout = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      pinned = Prelude.Nothing,
      executable = Prelude.Nothing,
      environment = Prelude.Nothing
    }

-- | The execution arguments.
functionConfiguration_execArgs :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_execArgs = Lens.lens (\FunctionConfiguration' {execArgs} -> execArgs) (\s@FunctionConfiguration' {} a -> s {execArgs = a} :: FunctionConfiguration)

-- | The memory size, in KB, which the function requires. This setting is not
-- applicable and should be cleared when you run the Lambda function
-- without containerization.
functionConfiguration_memorySize :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Int)
functionConfiguration_memorySize = Lens.lens (\FunctionConfiguration' {memorySize} -> memorySize) (\s@FunctionConfiguration' {} a -> s {memorySize = a} :: FunctionConfiguration)

-- | The allowed function execution time, after which Lambda should terminate
-- the function. This timeout still applies to pinned Lambda functions for
-- each request.
functionConfiguration_timeout :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Int)
functionConfiguration_timeout = Lens.lens (\FunctionConfiguration' {timeout} -> timeout) (\s@FunctionConfiguration' {} a -> s {timeout = a} :: FunctionConfiguration)

-- | The expected encoding type of the input payload for the function. The
-- default is \'\'json\'\'.
functionConfiguration_encodingType :: Lens.Lens' FunctionConfiguration (Prelude.Maybe EncodingType)
functionConfiguration_encodingType = Lens.lens (\FunctionConfiguration' {encodingType} -> encodingType) (\s@FunctionConfiguration' {} a -> s {encodingType = a} :: FunctionConfiguration)

-- | True if the function is pinned. Pinned means the function is long-lived
-- and starts when the core starts.
functionConfiguration_pinned :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Bool)
functionConfiguration_pinned = Lens.lens (\FunctionConfiguration' {pinned} -> pinned) (\s@FunctionConfiguration' {} a -> s {pinned = a} :: FunctionConfiguration)

-- | The name of the function executable.
functionConfiguration_executable :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_executable = Lens.lens (\FunctionConfiguration' {executable} -> executable) (\s@FunctionConfiguration' {} a -> s {executable = a} :: FunctionConfiguration)

-- | The environment configuration of the function.
functionConfiguration_environment :: Lens.Lens' FunctionConfiguration (Prelude.Maybe FunctionConfigurationEnvironment)
functionConfiguration_environment = Lens.lens (\FunctionConfiguration' {environment} -> environment) (\s@FunctionConfiguration' {} a -> s {environment = a} :: FunctionConfiguration)

instance Prelude.FromJSON FunctionConfiguration where
  parseJSON =
    Prelude.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Prelude.<$> (x Prelude..:? "ExecArgs")
            Prelude.<*> (x Prelude..:? "MemorySize")
            Prelude.<*> (x Prelude..:? "Timeout")
            Prelude.<*> (x Prelude..:? "EncodingType")
            Prelude.<*> (x Prelude..:? "Pinned")
            Prelude.<*> (x Prelude..:? "Executable")
            Prelude.<*> (x Prelude..:? "Environment")
      )

instance Prelude.Hashable FunctionConfiguration

instance Prelude.NFData FunctionConfiguration

instance Prelude.ToJSON FunctionConfiguration where
  toJSON FunctionConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ExecArgs" Prelude..=) Prelude.<$> execArgs,
            ("MemorySize" Prelude..=) Prelude.<$> memorySize,
            ("Timeout" Prelude..=) Prelude.<$> timeout,
            ("EncodingType" Prelude..=) Prelude.<$> encodingType,
            ("Pinned" Prelude..=) Prelude.<$> pinned,
            ("Executable" Prelude..=) Prelude.<$> executable,
            ("Environment" Prelude..=) Prelude.<$> environment
          ]
      )
