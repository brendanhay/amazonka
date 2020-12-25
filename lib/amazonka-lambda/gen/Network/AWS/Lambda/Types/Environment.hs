{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Environment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Environment
  ( Environment (..),

    -- * Smart constructor
    mkEnvironment,

    -- * Lenses
    eVariables,
  )
where

import qualified Network.AWS.Lambda.Types.EnvironmentVariableName as Types
import qualified Network.AWS.Lambda.Types.EnvironmentVariableValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A function's environment variable settings.
--
-- /See:/ 'mkEnvironment' smart constructor.
newtype Environment = Environment'
  { -- | Environment variable key-value pairs.
    variables :: Core.Maybe (Core.HashMap Types.EnvironmentVariableName Types.EnvironmentVariableValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Environment' value with any optional fields omitted.
mkEnvironment ::
  Environment
mkEnvironment = Environment' {variables = Core.Nothing}

-- | Environment variable key-value pairs.
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eVariables :: Lens.Lens' Environment (Core.Maybe (Core.HashMap Types.EnvironmentVariableName Types.EnvironmentVariableValue))
eVariables = Lens.field @"variables"
{-# DEPRECATED eVariables "Use generic-lens or generic-optics with 'variables' instead." #-}

instance Core.FromJSON Environment where
  toJSON Environment {..} =
    Core.object
      (Core.catMaybes [("Variables" Core..=) Core.<$> variables])
