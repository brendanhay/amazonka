{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.FailureException
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.FailureException
  ( FailureException (..)
  -- * Smart constructor
  , mkFailureException
  -- * Lenses
  , feExceptionDescription
  , feExceptionName
  ) where

import qualified Network.AWS.DynamoDB.Types.ExceptionDescription as Types
import qualified Network.AWS.DynamoDB.Types.ExceptionName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a failure a contributor insights operation.
--
-- /See:/ 'mkFailureException' smart constructor.
data FailureException = FailureException'
  { exceptionDescription :: Core.Maybe Types.ExceptionDescription
    -- ^ Description of the failure.
  , exceptionName :: Core.Maybe Types.ExceptionName
    -- ^ Exception name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailureException' value with any optional fields omitted.
mkFailureException
    :: FailureException
mkFailureException
  = FailureException'{exceptionDescription = Core.Nothing,
                      exceptionName = Core.Nothing}

-- | Description of the failure.
--
-- /Note:/ Consider using 'exceptionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feExceptionDescription :: Lens.Lens' FailureException (Core.Maybe Types.ExceptionDescription)
feExceptionDescription = Lens.field @"exceptionDescription"
{-# INLINEABLE feExceptionDescription #-}
{-# DEPRECATED exceptionDescription "Use generic-lens or generic-optics with 'exceptionDescription' instead"  #-}

-- | Exception name.
--
-- /Note:/ Consider using 'exceptionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
feExceptionName :: Lens.Lens' FailureException (Core.Maybe Types.ExceptionName)
feExceptionName = Lens.field @"exceptionName"
{-# INLINEABLE feExceptionName #-}
{-# DEPRECATED exceptionName "Use generic-lens or generic-optics with 'exceptionName' instead"  #-}

instance Core.FromJSON FailureException where
        parseJSON
          = Core.withObject "FailureException" Core.$
              \ x ->
                FailureException' Core.<$>
                  (x Core..:? "ExceptionDescription") Core.<*>
                    x Core..:? "ExceptionName"
