{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchReadException
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchReadException
  ( BatchReadException (..),

    -- * Smart constructor
    mkBatchReadException,

    -- * Lenses
    breMessage,
    breType,
  )
where

import qualified Network.AWS.CloudDirectory.Types.BatchReadExceptionType as Types
import qualified Network.AWS.CloudDirectory.Types.ExceptionMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The batch read exception structure, which contains the exception type and message.
--
-- /See:/ 'mkBatchReadException' smart constructor.
data BatchReadException = BatchReadException'
  { -- | An exception message that is associated with the failure.
    message :: Core.Maybe Types.ExceptionMessage,
    -- | A type of exception, such as @InvalidArnException@ .
    type' :: Core.Maybe Types.BatchReadExceptionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchReadException' value with any optional fields omitted.
mkBatchReadException ::
  BatchReadException
mkBatchReadException =
  BatchReadException' {message = Core.Nothing, type' = Core.Nothing}

-- | An exception message that is associated with the failure.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
breMessage :: Lens.Lens' BatchReadException (Core.Maybe Types.ExceptionMessage)
breMessage = Lens.field @"message"
{-# DEPRECATED breMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | A type of exception, such as @InvalidArnException@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
breType :: Lens.Lens' BatchReadException (Core.Maybe Types.BatchReadExceptionType)
breType = Lens.field @"type'"
{-# DEPRECATED breType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON BatchReadException where
  parseJSON =
    Core.withObject "BatchReadException" Core.$
      \x ->
        BatchReadException'
          Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "Type")
