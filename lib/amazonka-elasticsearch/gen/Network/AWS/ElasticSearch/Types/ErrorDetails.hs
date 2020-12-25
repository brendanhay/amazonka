{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ErrorDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ErrorDetails
  ( ErrorDetails (..),

    -- * Smart constructor
    mkErrorDetails,

    -- * Lenses
    edErrorMessage,
    edErrorType,
  )
where

import qualified Network.AWS.ElasticSearch.Types.ErrorMessage as Types
import qualified Network.AWS.ElasticSearch.Types.ErrorType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { errorMessage :: Core.Maybe Types.ErrorMessage,
    errorType :: Core.Maybe Types.ErrorType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorDetails' value with any optional fields omitted.
mkErrorDetails ::
  ErrorDetails
mkErrorDetails =
  ErrorDetails'
    { errorMessage = Core.Nothing,
      errorType = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edErrorMessage :: Lens.Lens' ErrorDetails (Core.Maybe Types.ErrorMessage)
edErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED edErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'errorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edErrorType :: Lens.Lens' ErrorDetails (Core.Maybe Types.ErrorType)
edErrorType = Lens.field @"errorType"
{-# DEPRECATED edErrorType "Use generic-lens or generic-optics with 'errorType' instead." #-}

instance Core.FromJSON ErrorDetails where
  parseJSON =
    Core.withObject "ErrorDetails" Core.$
      \x ->
        ErrorDetails'
          Core.<$> (x Core..:? "ErrorMessage") Core.<*> (x Core..:? "ErrorType")
