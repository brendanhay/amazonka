{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadOperationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadOperationResponse
  ( BatchReadOperationResponse (..),

    -- * Smart constructor
    mkBatchReadOperationResponse,

    -- * Lenses
    brorExceptionResponse,
    brorSuccessfulResponse,
  )
where

import qualified Network.AWS.CloudDirectory.Types.BatchReadException as Types
import qualified Network.AWS.CloudDirectory.Types.BatchReadSuccessfulResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a @BatchRead@ response operation.
--
-- /See:/ 'mkBatchReadOperationResponse' smart constructor.
data BatchReadOperationResponse = BatchReadOperationResponse'
  { -- | Identifies which operation in a batch has failed.
    exceptionResponse :: Core.Maybe Types.BatchReadException,
    -- | Identifies which operation in a batch has succeeded.
    successfulResponse :: Core.Maybe Types.BatchReadSuccessfulResponse
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BatchReadOperationResponse' value with any optional fields omitted.
mkBatchReadOperationResponse ::
  BatchReadOperationResponse
mkBatchReadOperationResponse =
  BatchReadOperationResponse'
    { exceptionResponse = Core.Nothing,
      successfulResponse = Core.Nothing
    }

-- | Identifies which operation in a batch has failed.
--
-- /Note:/ Consider using 'exceptionResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brorExceptionResponse :: Lens.Lens' BatchReadOperationResponse (Core.Maybe Types.BatchReadException)
brorExceptionResponse = Lens.field @"exceptionResponse"
{-# DEPRECATED brorExceptionResponse "Use generic-lens or generic-optics with 'exceptionResponse' instead." #-}

-- | Identifies which operation in a batch has succeeded.
--
-- /Note:/ Consider using 'successfulResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brorSuccessfulResponse :: Lens.Lens' BatchReadOperationResponse (Core.Maybe Types.BatchReadSuccessfulResponse)
brorSuccessfulResponse = Lens.field @"successfulResponse"
{-# DEPRECATED brorSuccessfulResponse "Use generic-lens or generic-optics with 'successfulResponse' instead." #-}

instance Core.FromJSON BatchReadOperationResponse where
  parseJSON =
    Core.withObject "BatchReadOperationResponse" Core.$
      \x ->
        BatchReadOperationResponse'
          Core.<$> (x Core..:? "ExceptionResponse")
          Core.<*> (x Core..:? "SuccessfulResponse")
