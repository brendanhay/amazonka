{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EnvironmentResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.EnvironmentResponse
  ( EnvironmentResponse (..)
  -- * Smart constructor
  , mkEnvironmentResponse
  -- * Lenses
  , erError
  , erVariables
  ) where

import qualified Network.AWS.Lambda.Types.EnvironmentError as Types
import qualified Network.AWS.Lambda.Types.EnvironmentVariableName as Types
import qualified Network.AWS.Lambda.Types.EnvironmentVariableValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The results of an operation to update or read environment variables. If the operation is successful, the response contains the environment variables. If it failed, the response contains details about the error.
--
-- /See:/ 'mkEnvironmentResponse' smart constructor.
data EnvironmentResponse = EnvironmentResponse'
  { error :: Core.Maybe Types.EnvironmentError
    -- ^ Error messages for environment variables that couldn't be applied.
  , variables :: Core.Maybe (Core.HashMap Types.EnvironmentVariableName Types.EnvironmentVariableValue)
    -- ^ Environment variable key-value pairs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentResponse' value with any optional fields omitted.
mkEnvironmentResponse
    :: EnvironmentResponse
mkEnvironmentResponse
  = EnvironmentResponse'{error = Core.Nothing,
                         variables = Core.Nothing}

-- | Error messages for environment variables that couldn't be applied.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erError :: Lens.Lens' EnvironmentResponse (Core.Maybe Types.EnvironmentError)
erError = Lens.field @"error"
{-# INLINEABLE erError #-}
{-# DEPRECATED error "Use generic-lens or generic-optics with 'error' instead"  #-}

-- | Environment variable key-value pairs.
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erVariables :: Lens.Lens' EnvironmentResponse (Core.Maybe (Core.HashMap Types.EnvironmentVariableName Types.EnvironmentVariableValue))
erVariables = Lens.field @"variables"
{-# INLINEABLE erVariables #-}
{-# DEPRECATED variables "Use generic-lens or generic-optics with 'variables' instead"  #-}

instance Core.FromJSON EnvironmentResponse where
        parseJSON
          = Core.withObject "EnvironmentResponse" Core.$
              \ x ->
                EnvironmentResponse' Core.<$>
                  (x Core..:? "Error") Core.<*> x Core..:? "Variables"
