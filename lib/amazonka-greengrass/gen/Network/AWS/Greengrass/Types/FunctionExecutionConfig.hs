-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionExecutionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionExecutionConfig
  ( FunctionExecutionConfig (..),

    -- * Smart constructor
    mkFunctionExecutionConfig,

    -- * Lenses
    fecRunAs,
    fecIsolationMode,
  )
where

import Network.AWS.Greengrass.Types.FunctionIsolationMode
import Network.AWS.Greengrass.Types.FunctionRunAsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information that specifies how a Lambda function runs.
--
-- /See:/ 'mkFunctionExecutionConfig' smart constructor.
data FunctionExecutionConfig = FunctionExecutionConfig'
  { runAs ::
      Lude.Maybe FunctionRunAsConfig,
    isolationMode ::
      Lude.Maybe FunctionIsolationMode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionExecutionConfig' with the minimum fields required to make a request.
--
-- * 'isolationMode' - Undocumented field.
-- * 'runAs' - Undocumented field.
mkFunctionExecutionConfig ::
  FunctionExecutionConfig
mkFunctionExecutionConfig =
  FunctionExecutionConfig'
    { runAs = Lude.Nothing,
      isolationMode = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'runAs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fecRunAs :: Lens.Lens' FunctionExecutionConfig (Lude.Maybe FunctionRunAsConfig)
fecRunAs = Lens.lens (runAs :: FunctionExecutionConfig -> Lude.Maybe FunctionRunAsConfig) (\s a -> s {runAs = a} :: FunctionExecutionConfig)
{-# DEPRECATED fecRunAs "Use generic-lens or generic-optics with 'runAs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'isolationMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fecIsolationMode :: Lens.Lens' FunctionExecutionConfig (Lude.Maybe FunctionIsolationMode)
fecIsolationMode = Lens.lens (isolationMode :: FunctionExecutionConfig -> Lude.Maybe FunctionIsolationMode) (\s a -> s {isolationMode = a} :: FunctionExecutionConfig)
{-# DEPRECATED fecIsolationMode "Use generic-lens or generic-optics with 'isolationMode' instead." #-}

instance Lude.FromJSON FunctionExecutionConfig where
  parseJSON =
    Lude.withObject
      "FunctionExecutionConfig"
      ( \x ->
          FunctionExecutionConfig'
            Lude.<$> (x Lude..:? "RunAs") Lude.<*> (x Lude..:? "IsolationMode")
      )

instance Lude.ToJSON FunctionExecutionConfig where
  toJSON FunctionExecutionConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RunAs" Lude..=) Lude.<$> runAs,
            ("IsolationMode" Lude..=) Lude.<$> isolationMode
          ]
      )
