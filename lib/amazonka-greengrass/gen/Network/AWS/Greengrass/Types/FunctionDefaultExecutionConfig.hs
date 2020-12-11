-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
  ( FunctionDefaultExecutionConfig (..),

    -- * Smart constructor
    mkFunctionDefaultExecutionConfig,

    -- * Lenses
    fdecRunAs,
    fdecIsolationMode,
  )
where

import Network.AWS.Greengrass.Types.FunctionIsolationMode
import Network.AWS.Greengrass.Types.FunctionRunAsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information that specifies how a Lambda function runs.
--
-- /See:/ 'mkFunctionDefaultExecutionConfig' smart constructor.
data FunctionDefaultExecutionConfig = FunctionDefaultExecutionConfig'
  { runAs ::
      Lude.Maybe
        FunctionRunAsConfig,
    isolationMode ::
      Lude.Maybe
        FunctionIsolationMode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionDefaultExecutionConfig' with the minimum fields required to make a request.
--
-- * 'isolationMode' - Undocumented field.
-- * 'runAs' - Undocumented field.
mkFunctionDefaultExecutionConfig ::
  FunctionDefaultExecutionConfig
mkFunctionDefaultExecutionConfig =
  FunctionDefaultExecutionConfig'
    { runAs = Lude.Nothing,
      isolationMode = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'runAs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdecRunAs :: Lens.Lens' FunctionDefaultExecutionConfig (Lude.Maybe FunctionRunAsConfig)
fdecRunAs = Lens.lens (runAs :: FunctionDefaultExecutionConfig -> Lude.Maybe FunctionRunAsConfig) (\s a -> s {runAs = a} :: FunctionDefaultExecutionConfig)
{-# DEPRECATED fdecRunAs "Use generic-lens or generic-optics with 'runAs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'isolationMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdecIsolationMode :: Lens.Lens' FunctionDefaultExecutionConfig (Lude.Maybe FunctionIsolationMode)
fdecIsolationMode = Lens.lens (isolationMode :: FunctionDefaultExecutionConfig -> Lude.Maybe FunctionIsolationMode) (\s a -> s {isolationMode = a} :: FunctionDefaultExecutionConfig)
{-# DEPRECATED fdecIsolationMode "Use generic-lens or generic-optics with 'isolationMode' instead." #-}

instance Lude.FromJSON FunctionDefaultExecutionConfig where
  parseJSON =
    Lude.withObject
      "FunctionDefaultExecutionConfig"
      ( \x ->
          FunctionDefaultExecutionConfig'
            Lude.<$> (x Lude..:? "RunAs") Lude.<*> (x Lude..:? "IsolationMode")
      )

instance Lude.ToJSON FunctionDefaultExecutionConfig where
  toJSON FunctionDefaultExecutionConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RunAs" Lude..=) Lude.<$> runAs,
            ("IsolationMode" Lude..=) Lude.<$> isolationMode
          ]
      )
