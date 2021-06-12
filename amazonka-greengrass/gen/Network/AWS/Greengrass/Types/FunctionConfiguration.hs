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

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.EncodingType
import Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
import qualified Network.AWS.Lens as Lens

-- | The configuration of the Lambda function.
--
-- /See:/ 'newFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The execution arguments.
    execArgs :: Core.Maybe Core.Text,
    -- | The memory size, in KB, which the function requires. This setting is not
    -- applicable and should be cleared when you run the Lambda function
    -- without containerization.
    memorySize :: Core.Maybe Core.Int,
    -- | The allowed function execution time, after which Lambda should terminate
    -- the function. This timeout still applies to pinned Lambda functions for
    -- each request.
    timeout :: Core.Maybe Core.Int,
    -- | The expected encoding type of the input payload for the function. The
    -- default is \'\'json\'\'.
    encodingType :: Core.Maybe EncodingType,
    -- | True if the function is pinned. Pinned means the function is long-lived
    -- and starts when the core starts.
    pinned :: Core.Maybe Core.Bool,
    -- | The name of the function executable.
    executable :: Core.Maybe Core.Text,
    -- | The environment configuration of the function.
    environment :: Core.Maybe FunctionConfigurationEnvironment
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { execArgs = Core.Nothing,
      memorySize = Core.Nothing,
      timeout = Core.Nothing,
      encodingType = Core.Nothing,
      pinned = Core.Nothing,
      executable = Core.Nothing,
      environment = Core.Nothing
    }

-- | The execution arguments.
functionConfiguration_execArgs :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Text)
functionConfiguration_execArgs = Lens.lens (\FunctionConfiguration' {execArgs} -> execArgs) (\s@FunctionConfiguration' {} a -> s {execArgs = a} :: FunctionConfiguration)

-- | The memory size, in KB, which the function requires. This setting is not
-- applicable and should be cleared when you run the Lambda function
-- without containerization.
functionConfiguration_memorySize :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Int)
functionConfiguration_memorySize = Lens.lens (\FunctionConfiguration' {memorySize} -> memorySize) (\s@FunctionConfiguration' {} a -> s {memorySize = a} :: FunctionConfiguration)

-- | The allowed function execution time, after which Lambda should terminate
-- the function. This timeout still applies to pinned Lambda functions for
-- each request.
functionConfiguration_timeout :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Int)
functionConfiguration_timeout = Lens.lens (\FunctionConfiguration' {timeout} -> timeout) (\s@FunctionConfiguration' {} a -> s {timeout = a} :: FunctionConfiguration)

-- | The expected encoding type of the input payload for the function. The
-- default is \'\'json\'\'.
functionConfiguration_encodingType :: Lens.Lens' FunctionConfiguration (Core.Maybe EncodingType)
functionConfiguration_encodingType = Lens.lens (\FunctionConfiguration' {encodingType} -> encodingType) (\s@FunctionConfiguration' {} a -> s {encodingType = a} :: FunctionConfiguration)

-- | True if the function is pinned. Pinned means the function is long-lived
-- and starts when the core starts.
functionConfiguration_pinned :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Bool)
functionConfiguration_pinned = Lens.lens (\FunctionConfiguration' {pinned} -> pinned) (\s@FunctionConfiguration' {} a -> s {pinned = a} :: FunctionConfiguration)

-- | The name of the function executable.
functionConfiguration_executable :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Text)
functionConfiguration_executable = Lens.lens (\FunctionConfiguration' {executable} -> executable) (\s@FunctionConfiguration' {} a -> s {executable = a} :: FunctionConfiguration)

-- | The environment configuration of the function.
functionConfiguration_environment :: Lens.Lens' FunctionConfiguration (Core.Maybe FunctionConfigurationEnvironment)
functionConfiguration_environment = Lens.lens (\FunctionConfiguration' {environment} -> environment) (\s@FunctionConfiguration' {} a -> s {environment = a} :: FunctionConfiguration)

instance Core.FromJSON FunctionConfiguration where
  parseJSON =
    Core.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Core.<$> (x Core..:? "ExecArgs")
            Core.<*> (x Core..:? "MemorySize")
            Core.<*> (x Core..:? "Timeout")
            Core.<*> (x Core..:? "EncodingType")
            Core.<*> (x Core..:? "Pinned")
            Core.<*> (x Core..:? "Executable")
            Core.<*> (x Core..:? "Environment")
      )

instance Core.Hashable FunctionConfiguration

instance Core.NFData FunctionConfiguration

instance Core.ToJSON FunctionConfiguration where
  toJSON FunctionConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExecArgs" Core..=) Core.<$> execArgs,
            ("MemorySize" Core..=) Core.<$> memorySize,
            ("Timeout" Core..=) Core.<$> timeout,
            ("EncodingType" Core..=) Core.<$> encodingType,
            ("Pinned" Core..=) Core.<$> pinned,
            ("Executable" Core..=) Core.<$> executable,
            ("Environment" Core..=) Core.<$> environment
          ]
      )
