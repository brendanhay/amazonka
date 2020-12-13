{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionConfiguration
  ( FunctionConfiguration (..),

    -- * Smart constructor
    mkFunctionConfiguration,

    -- * Lenses
    fcMemorySize,
    fcExecArgs,
    fcEnvironment,
    fcExecutable,
    fcPinned,
    fcEncodingType,
    fcTimeout,
  )
where

import Network.AWS.Greengrass.Types.EncodingType
import Network.AWS.Greengrass.Types.FunctionConfigurationEnvironment
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The configuration of the Lambda function.
--
-- /See:/ 'mkFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The memory size, in KB, which the function requires. This setting is not applicable and should be cleared when you run the Lambda function without containerization.
    memorySize :: Lude.Maybe Lude.Int,
    -- | The execution arguments.
    execArgs :: Lude.Maybe Lude.Text,
    -- | The environment configuration of the function.
    environment :: Lude.Maybe FunctionConfigurationEnvironment,
    -- | The name of the function executable.
    executable :: Lude.Maybe Lude.Text,
    -- | True if the function is pinned. Pinned means the function is long-lived and starts when the core starts.
    pinned :: Lude.Maybe Lude.Bool,
    -- | The expected encoding type of the input payload for the function. The default is ''json''.
    encodingType :: Lude.Maybe EncodingType,
    -- | The allowed function execution time, after which Lambda should terminate the function. This timeout still applies to pinned Lambda functions for each request.
    timeout :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionConfiguration' with the minimum fields required to make a request.
--
-- * 'memorySize' - The memory size, in KB, which the function requires. This setting is not applicable and should be cleared when you run the Lambda function without containerization.
-- * 'execArgs' - The execution arguments.
-- * 'environment' - The environment configuration of the function.
-- * 'executable' - The name of the function executable.
-- * 'pinned' - True if the function is pinned. Pinned means the function is long-lived and starts when the core starts.
-- * 'encodingType' - The expected encoding type of the input payload for the function. The default is ''json''.
-- * 'timeout' - The allowed function execution time, after which Lambda should terminate the function. This timeout still applies to pinned Lambda functions for each request.
mkFunctionConfiguration ::
  FunctionConfiguration
mkFunctionConfiguration =
  FunctionConfiguration'
    { memorySize = Lude.Nothing,
      execArgs = Lude.Nothing,
      environment = Lude.Nothing,
      executable = Lude.Nothing,
      pinned = Lude.Nothing,
      encodingType = Lude.Nothing,
      timeout = Lude.Nothing
    }

-- | The memory size, in KB, which the function requires. This setting is not applicable and should be cleared when you run the Lambda function without containerization.
--
-- /Note:/ Consider using 'memorySize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcMemorySize :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Int)
fcMemorySize = Lens.lens (memorySize :: FunctionConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {memorySize = a} :: FunctionConfiguration)
{-# DEPRECATED fcMemorySize "Use generic-lens or generic-optics with 'memorySize' instead." #-}

-- | The execution arguments.
--
-- /Note:/ Consider using 'execArgs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcExecArgs :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcExecArgs = Lens.lens (execArgs :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {execArgs = a} :: FunctionConfiguration)
{-# DEPRECATED fcExecArgs "Use generic-lens or generic-optics with 'execArgs' instead." #-}

-- | The environment configuration of the function.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcEnvironment :: Lens.Lens' FunctionConfiguration (Lude.Maybe FunctionConfigurationEnvironment)
fcEnvironment = Lens.lens (environment :: FunctionConfiguration -> Lude.Maybe FunctionConfigurationEnvironment) (\s a -> s {environment = a} :: FunctionConfiguration)
{-# DEPRECATED fcEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The name of the function executable.
--
-- /Note:/ Consider using 'executable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcExecutable :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Text)
fcExecutable = Lens.lens (executable :: FunctionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {executable = a} :: FunctionConfiguration)
{-# DEPRECATED fcExecutable "Use generic-lens or generic-optics with 'executable' instead." #-}

-- | True if the function is pinned. Pinned means the function is long-lived and starts when the core starts.
--
-- /Note:/ Consider using 'pinned' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcPinned :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Bool)
fcPinned = Lens.lens (pinned :: FunctionConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {pinned = a} :: FunctionConfiguration)
{-# DEPRECATED fcPinned "Use generic-lens or generic-optics with 'pinned' instead." #-}

-- | The expected encoding type of the input payload for the function. The default is ''json''.
--
-- /Note:/ Consider using 'encodingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcEncodingType :: Lens.Lens' FunctionConfiguration (Lude.Maybe EncodingType)
fcEncodingType = Lens.lens (encodingType :: FunctionConfiguration -> Lude.Maybe EncodingType) (\s a -> s {encodingType = a} :: FunctionConfiguration)
{-# DEPRECATED fcEncodingType "Use generic-lens or generic-optics with 'encodingType' instead." #-}

-- | The allowed function execution time, after which Lambda should terminate the function. This timeout still applies to pinned Lambda functions for each request.
--
-- /Note:/ Consider using 'timeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcTimeout :: Lens.Lens' FunctionConfiguration (Lude.Maybe Lude.Int)
fcTimeout = Lens.lens (timeout :: FunctionConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {timeout = a} :: FunctionConfiguration)
{-# DEPRECATED fcTimeout "Use generic-lens or generic-optics with 'timeout' instead." #-}

instance Lude.FromJSON FunctionConfiguration where
  parseJSON =
    Lude.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Lude.<$> (x Lude..:? "MemorySize")
            Lude.<*> (x Lude..:? "ExecArgs")
            Lude.<*> (x Lude..:? "Environment")
            Lude.<*> (x Lude..:? "Executable")
            Lude.<*> (x Lude..:? "Pinned")
            Lude.<*> (x Lude..:? "EncodingType")
            Lude.<*> (x Lude..:? "Timeout")
      )

instance Lude.ToJSON FunctionConfiguration where
  toJSON FunctionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MemorySize" Lude..=) Lude.<$> memorySize,
            ("ExecArgs" Lude..=) Lude.<$> execArgs,
            ("Environment" Lude..=) Lude.<$> environment,
            ("Executable" Lude..=) Lude.<$> executable,
            ("Pinned" Lude..=) Lude.<$> pinned,
            ("EncodingType" Lude..=) Lude.<$> encodingType,
            ("Timeout" Lude..=) Lude.<$> timeout
          ]
      )
