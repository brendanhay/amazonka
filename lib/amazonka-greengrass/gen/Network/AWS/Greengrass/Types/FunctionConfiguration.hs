{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.FunctionConfiguration
  ( FunctionConfiguration (..)
  -- * Smart constructor
  , mkFunctionConfiguration
  -- * Lenses
  , fcEncodingType
  , fcEnvironment
  , fcExecArgs
  , fcExecutable
  , fcMemorySize
  , fcPinned
  , fcTimeout
  ) where

import qualified Network.AWS.Greengrass.Types.EncodingType as Types
import qualified Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The configuration of the Lambda function.
--
-- /See:/ 'mkFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { encodingType :: Core.Maybe Types.EncodingType
    -- ^ The expected encoding type of the input payload for the function. The default is ''json''.
  , environment :: Core.Maybe Types.FunctionConfigurationEnvironment
    -- ^ The environment configuration of the function.
  , execArgs :: Core.Maybe Core.Text
    -- ^ The execution arguments.
  , executable :: Core.Maybe Core.Text
    -- ^ The name of the function executable.
  , memorySize :: Core.Maybe Core.Int
    -- ^ The memory size, in KB, which the function requires. This setting is not applicable and should be cleared when you run the Lambda function without containerization.
  , pinned :: Core.Maybe Core.Bool
    -- ^ True if the function is pinned. Pinned means the function is long-lived and starts when the core starts.
  , timeout :: Core.Maybe Core.Int
    -- ^ The allowed function execution time, after which Lambda should terminate the function. This timeout still applies to pinned Lambda functions for each request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionConfiguration' value with any optional fields omitted.
mkFunctionConfiguration
    :: FunctionConfiguration
mkFunctionConfiguration
  = FunctionConfiguration'{encodingType = Core.Nothing,
                           environment = Core.Nothing, execArgs = Core.Nothing,
                           executable = Core.Nothing, memorySize = Core.Nothing,
                           pinned = Core.Nothing, timeout = Core.Nothing}

-- | The expected encoding type of the input payload for the function. The default is ''json''.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcEncodingType :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.EncodingType)
fcEncodingType = Lens.field @"encodingType"
{-# INLINEABLE fcEncodingType #-}
{-# DEPRECATED encodingType "Use generic-lens or generic-optics with 'encodingType' instead"  #-}

-- | The environment configuration of the function.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcEnvironment :: Lens.Lens' FunctionConfiguration (Core.Maybe Types.FunctionConfigurationEnvironment)
fcEnvironment = Lens.field @"environment"
{-# INLINEABLE fcEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | The execution arguments.
--
-- /Note:/ Consider using 'execArgs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcExecArgs :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Text)
fcExecArgs = Lens.field @"execArgs"
{-# INLINEABLE fcExecArgs #-}
{-# DEPRECATED execArgs "Use generic-lens or generic-optics with 'execArgs' instead"  #-}

-- | The name of the function executable.
--
-- /Note:/ Consider using 'executable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcExecutable :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Text)
fcExecutable = Lens.field @"executable"
{-# INLINEABLE fcExecutable #-}
{-# DEPRECATED executable "Use generic-lens or generic-optics with 'executable' instead"  #-}

-- | The memory size, in KB, which the function requires. This setting is not applicable and should be cleared when you run the Lambda function without containerization.
--
-- /Note:/ Consider using 'memorySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcMemorySize :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Int)
fcMemorySize = Lens.field @"memorySize"
{-# INLINEABLE fcMemorySize #-}
{-# DEPRECATED memorySize "Use generic-lens or generic-optics with 'memorySize' instead"  #-}

-- | True if the function is pinned. Pinned means the function is long-lived and starts when the core starts.
--
-- /Note:/ Consider using 'pinned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcPinned :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Bool)
fcPinned = Lens.field @"pinned"
{-# INLINEABLE fcPinned #-}
{-# DEPRECATED pinned "Use generic-lens or generic-optics with 'pinned' instead"  #-}

-- | The allowed function execution time, after which Lambda should terminate the function. This timeout still applies to pinned Lambda functions for each request.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcTimeout :: Lens.Lens' FunctionConfiguration (Core.Maybe Core.Int)
fcTimeout = Lens.field @"timeout"
{-# INLINEABLE fcTimeout #-}
{-# DEPRECATED timeout "Use generic-lens or generic-optics with 'timeout' instead"  #-}

instance Core.FromJSON FunctionConfiguration where
        toJSON FunctionConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("EncodingType" Core..=) Core.<$> encodingType,
                  ("Environment" Core..=) Core.<$> environment,
                  ("ExecArgs" Core..=) Core.<$> execArgs,
                  ("Executable" Core..=) Core.<$> executable,
                  ("MemorySize" Core..=) Core.<$> memorySize,
                  ("Pinned" Core..=) Core.<$> pinned,
                  ("Timeout" Core..=) Core.<$> timeout])

instance Core.FromJSON FunctionConfiguration where
        parseJSON
          = Core.withObject "FunctionConfiguration" Core.$
              \ x ->
                FunctionConfiguration' Core.<$>
                  (x Core..:? "EncodingType") Core.<*> x Core..:? "Environment"
                    Core.<*> x Core..:? "ExecArgs"
                    Core.<*> x Core..:? "Executable"
                    Core.<*> x Core..:? "MemorySize"
                    Core.<*> x Core..:? "Pinned"
                    Core.<*> x Core..:? "Timeout"
