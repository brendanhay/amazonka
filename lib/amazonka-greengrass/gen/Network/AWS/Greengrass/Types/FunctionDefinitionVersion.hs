-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionDefinitionVersion
  ( FunctionDefinitionVersion (..),

    -- * Smart constructor
    mkFunctionDefinitionVersion,

    -- * Lenses
    fdvDefaultConfig,
    fdvFunctions,
  )
where

import Network.AWS.Greengrass.Types.Function
import Network.AWS.Greengrass.Types.FunctionDefaultConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a function definition version.
--
-- /See:/ 'mkFunctionDefinitionVersion' smart constructor.
data FunctionDefinitionVersion = FunctionDefinitionVersion'
  { defaultConfig ::
      Lude.Maybe FunctionDefaultConfig,
    functions :: Lude.Maybe [Function]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionDefinitionVersion' with the minimum fields required to make a request.
--
-- * 'defaultConfig' - The default configuration that applies to all Lambda functions in this function definition version. Individual Lambda functions can override these settings.
-- * 'functions' - A list of Lambda functions in this function definition version.
mkFunctionDefinitionVersion ::
  FunctionDefinitionVersion
mkFunctionDefinitionVersion =
  FunctionDefinitionVersion'
    { defaultConfig = Lude.Nothing,
      functions = Lude.Nothing
    }

-- | The default configuration that applies to all Lambda functions in this function definition version. Individual Lambda functions can override these settings.
--
-- /Note:/ Consider using 'defaultConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdvDefaultConfig :: Lens.Lens' FunctionDefinitionVersion (Lude.Maybe FunctionDefaultConfig)
fdvDefaultConfig = Lens.lens (defaultConfig :: FunctionDefinitionVersion -> Lude.Maybe FunctionDefaultConfig) (\s a -> s {defaultConfig = a} :: FunctionDefinitionVersion)
{-# DEPRECATED fdvDefaultConfig "Use generic-lens or generic-optics with 'defaultConfig' instead." #-}

-- | A list of Lambda functions in this function definition version.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdvFunctions :: Lens.Lens' FunctionDefinitionVersion (Lude.Maybe [Function])
fdvFunctions = Lens.lens (functions :: FunctionDefinitionVersion -> Lude.Maybe [Function]) (\s a -> s {functions = a} :: FunctionDefinitionVersion)
{-# DEPRECATED fdvFunctions "Use generic-lens or generic-optics with 'functions' instead." #-}

instance Lude.FromJSON FunctionDefinitionVersion where
  parseJSON =
    Lude.withObject
      "FunctionDefinitionVersion"
      ( \x ->
          FunctionDefinitionVersion'
            Lude.<$> (x Lude..:? "DefaultConfig")
            Lude.<*> (x Lude..:? "Functions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON FunctionDefinitionVersion where
  toJSON FunctionDefinitionVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultConfig" Lude..=) Lude.<$> defaultConfig,
            ("Functions" Lude..=) Lude.<$> functions
          ]
      )
