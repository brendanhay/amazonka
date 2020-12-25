{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.FailureDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.FailureDetails
  ( FailureDetails (..),

    -- * Smart constructor
    mkFailureDetails,

    -- * Lenses
    fdType,
    fdMessage,
    fdExternalExecutionId,
  )
where

import qualified Network.AWS.CodePipeline.Types.ExecutionId as Types
import qualified Network.AWS.CodePipeline.Types.FailureType as Types
import qualified Network.AWS.CodePipeline.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about failure details.
--
-- /See:/ 'mkFailureDetails' smart constructor.
data FailureDetails = FailureDetails'
  { -- | The type of the failure.
    type' :: Types.FailureType,
    -- | The message about the failure.
    message :: Types.Message,
    -- | The external ID of the run of the action that failed.
    externalExecutionId :: Core.Maybe Types.ExecutionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailureDetails' value with any optional fields omitted.
mkFailureDetails ::
  -- | 'type\''
  Types.FailureType ->
  -- | 'message'
  Types.Message ->
  FailureDetails
mkFailureDetails type' message =
  FailureDetails'
    { type',
      message,
      externalExecutionId = Core.Nothing
    }

-- | The type of the failure.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdType :: Lens.Lens' FailureDetails Types.FailureType
fdType = Lens.field @"type'"
{-# DEPRECATED fdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The message about the failure.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdMessage :: Lens.Lens' FailureDetails Types.Message
fdMessage = Lens.field @"message"
{-# DEPRECATED fdMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The external ID of the run of the action that failed.
--
-- /Note:/ Consider using 'externalExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdExternalExecutionId :: Lens.Lens' FailureDetails (Core.Maybe Types.ExecutionId)
fdExternalExecutionId = Lens.field @"externalExecutionId"
{-# DEPRECATED fdExternalExecutionId "Use generic-lens or generic-optics with 'externalExecutionId' instead." #-}

instance Core.FromJSON FailureDetails where
  toJSON FailureDetails {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("type" Core..= type'),
            Core.Just ("message" Core..= message),
            ("externalExecutionId" Core..=) Core.<$> externalExecutionId
          ]
      )
