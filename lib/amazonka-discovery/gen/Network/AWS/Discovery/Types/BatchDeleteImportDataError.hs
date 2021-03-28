{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.BatchDeleteImportDataError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Discovery.Types.BatchDeleteImportDataError
  ( BatchDeleteImportDataError (..)
  -- * Smart constructor
  , mkBatchDeleteImportDataError
  -- * Lenses
  , bdideErrorCode
  , bdideErrorDescription
  , bdideImportTaskId
  ) where

import qualified Network.AWS.Discovery.Types.BatchDeleteImportDataErrorCode as Types
import qualified Network.AWS.Discovery.Types.ErrorDescription as Types
import qualified Network.AWS.Discovery.Types.ImportTaskId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Error messages returned for each import task that you deleted as a response for this command.
--
-- /See:/ 'mkBatchDeleteImportDataError' smart constructor.
data BatchDeleteImportDataError = BatchDeleteImportDataError'
  { errorCode :: Core.Maybe Types.BatchDeleteImportDataErrorCode
    -- ^ The type of error that occurred for a specific import task.
  , errorDescription :: Core.Maybe Types.ErrorDescription
    -- ^ The description of the error that occurred for a specific import task.
  , importTaskId :: Core.Maybe Types.ImportTaskId
    -- ^ The unique import ID associated with the error that occurred.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDeleteImportDataError' value with any optional fields omitted.
mkBatchDeleteImportDataError
    :: BatchDeleteImportDataError
mkBatchDeleteImportDataError
  = BatchDeleteImportDataError'{errorCode = Core.Nothing,
                                errorDescription = Core.Nothing, importTaskId = Core.Nothing}

-- | The type of error that occurred for a specific import task.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdideErrorCode :: Lens.Lens' BatchDeleteImportDataError (Core.Maybe Types.BatchDeleteImportDataErrorCode)
bdideErrorCode = Lens.field @"errorCode"
{-# INLINEABLE bdideErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The description of the error that occurred for a specific import task.
--
-- /Note:/ Consider using 'errorDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdideErrorDescription :: Lens.Lens' BatchDeleteImportDataError (Core.Maybe Types.ErrorDescription)
bdideErrorDescription = Lens.field @"errorDescription"
{-# INLINEABLE bdideErrorDescription #-}
{-# DEPRECATED errorDescription "Use generic-lens or generic-optics with 'errorDescription' instead"  #-}

-- | The unique import ID associated with the error that occurred.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdideImportTaskId :: Lens.Lens' BatchDeleteImportDataError (Core.Maybe Types.ImportTaskId)
bdideImportTaskId = Lens.field @"importTaskId"
{-# INLINEABLE bdideImportTaskId #-}
{-# DEPRECATED importTaskId "Use generic-lens or generic-optics with 'importTaskId' instead"  #-}

instance Core.FromJSON BatchDeleteImportDataError where
        parseJSON
          = Core.withObject "BatchDeleteImportDataError" Core.$
              \ x ->
                BatchDeleteImportDataError' Core.<$>
                  (x Core..:? "errorCode") Core.<*> x Core..:? "errorDescription"
                    Core.<*> x Core..:? "importTaskId"
