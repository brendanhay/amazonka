{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.LastReportGenerationExecutionError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.LastReportGenerationExecutionError
  ( LastReportGenerationExecutionError (..),

    -- * Smart constructor
    mkLastReportGenerationExecutionError,

    -- * Lenses
    lrgeeErrorCode,
    lrgeeErrorMessage,
  )
where

import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.AppStream.Types.UsageReportExecutionErrorCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the error that is returned when a usage report can't be generated.
--
-- /See:/ 'mkLastReportGenerationExecutionError' smart constructor.
data LastReportGenerationExecutionError = LastReportGenerationExecutionError'
  { -- | The error code for the error that is returned when a usage report can't be generated.
    errorCode :: Core.Maybe Types.UsageReportExecutionErrorCode,
    -- | The error message for the error that is returned when a usage report can't be generated.
    errorMessage :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LastReportGenerationExecutionError' value with any optional fields omitted.
mkLastReportGenerationExecutionError ::
  LastReportGenerationExecutionError
mkLastReportGenerationExecutionError =
  LastReportGenerationExecutionError'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing
    }

-- | The error code for the error that is returned when a usage report can't be generated.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgeeErrorCode :: Lens.Lens' LastReportGenerationExecutionError (Core.Maybe Types.UsageReportExecutionErrorCode)
lrgeeErrorCode = Lens.field @"errorCode"
{-# DEPRECATED lrgeeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message for the error that is returned when a usage report can't be generated.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrgeeErrorMessage :: Lens.Lens' LastReportGenerationExecutionError (Core.Maybe Types.String)
lrgeeErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED lrgeeErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Core.FromJSON LastReportGenerationExecutionError where
  parseJSON =
    Core.withObject "LastReportGenerationExecutionError" Core.$
      \x ->
        LastReportGenerationExecutionError'
          Core.<$> (x Core..:? "ErrorCode") Core.<*> (x Core..:? "ErrorMessage")
