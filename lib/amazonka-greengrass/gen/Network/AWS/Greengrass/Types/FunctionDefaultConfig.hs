{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionDefaultConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionDefaultConfig
  ( FunctionDefaultConfig (..),

    -- * Smart constructor
    mkFunctionDefaultConfig,

    -- * Lenses
    fdcExecution,
  )
where

import Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The default configuration that applies to all Lambda functions in the group. Individual Lambda functions can override these settings.
--
-- /See:/ 'mkFunctionDefaultConfig' smart constructor.
newtype FunctionDefaultConfig = FunctionDefaultConfig'
  { execution :: Lude.Maybe FunctionDefaultExecutionConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionDefaultConfig' with the minimum fields required to make a request.
--
-- * 'execution' -
mkFunctionDefaultConfig ::
  FunctionDefaultConfig
mkFunctionDefaultConfig =
  FunctionDefaultConfig' {execution = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdcExecution :: Lens.Lens' FunctionDefaultConfig (Lude.Maybe FunctionDefaultExecutionConfig)
fdcExecution = Lens.lens (execution :: FunctionDefaultConfig -> Lude.Maybe FunctionDefaultExecutionConfig) (\s a -> s {execution = a} :: FunctionDefaultConfig)
{-# DEPRECATED fdcExecution "Use generic-lens or generic-optics with 'execution' instead." #-}

instance Lude.FromJSON FunctionDefaultConfig where
  parseJSON =
    Lude.withObject
      "FunctionDefaultConfig"
      (\x -> FunctionDefaultConfig' Lude.<$> (x Lude..:? "Execution"))

instance Lude.ToJSON FunctionDefaultConfig where
  toJSON FunctionDefaultConfig' {..} =
    Lude.object
      (Lude.catMaybes [("Execution" Lude..=) Lude.<$> execution])
