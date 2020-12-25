{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.UnprocessedQueryExecutionId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.UnprocessedQueryExecutionId
  ( UnprocessedQueryExecutionId (..),

    -- * Smart constructor
    mkUnprocessedQueryExecutionId,

    -- * Lenses
    uqeiErrorCode,
    uqeiErrorMessage,
    uqeiQueryExecutionId,
  )
where

import qualified Network.AWS.Athena.Types.ErrorCode as Types
import qualified Network.AWS.Athena.Types.ErrorMessage as Types
import qualified Network.AWS.Athena.Types.QueryExecutionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a query execution that failed to process.
--
-- /See:/ 'mkUnprocessedQueryExecutionId' smart constructor.
data UnprocessedQueryExecutionId = UnprocessedQueryExecutionId'
  { -- | The error code returned when the query execution failed to process, if applicable.
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | The error message returned when the query execution failed to process, if applicable.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The unique identifier of the query execution.
    queryExecutionId :: Core.Maybe Types.QueryExecutionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UnprocessedQueryExecutionId' value with any optional fields omitted.
mkUnprocessedQueryExecutionId ::
  UnprocessedQueryExecutionId
mkUnprocessedQueryExecutionId =
  UnprocessedQueryExecutionId'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      queryExecutionId = Core.Nothing
    }

-- | The error code returned when the query execution failed to process, if applicable.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqeiErrorCode :: Lens.Lens' UnprocessedQueryExecutionId (Core.Maybe Types.ErrorCode)
uqeiErrorCode = Lens.field @"errorCode"
{-# DEPRECATED uqeiErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message returned when the query execution failed to process, if applicable.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqeiErrorMessage :: Lens.Lens' UnprocessedQueryExecutionId (Core.Maybe Types.ErrorMessage)
uqeiErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED uqeiErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The unique identifier of the query execution.
--
-- /Note:/ Consider using 'queryExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uqeiQueryExecutionId :: Lens.Lens' UnprocessedQueryExecutionId (Core.Maybe Types.QueryExecutionId)
uqeiQueryExecutionId = Lens.field @"queryExecutionId"
{-# DEPRECATED uqeiQueryExecutionId "Use generic-lens or generic-optics with 'queryExecutionId' instead." #-}

instance Core.FromJSON UnprocessedQueryExecutionId where
  parseJSON =
    Core.withObject "UnprocessedQueryExecutionId" Core.$
      \x ->
        UnprocessedQueryExecutionId'
          Core.<$> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "QueryExecutionId")
