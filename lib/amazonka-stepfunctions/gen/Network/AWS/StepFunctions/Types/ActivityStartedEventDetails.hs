{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ActivityStartedEventDetails
  ( ActivityStartedEventDetails (..),

    -- * Smart constructor
    mkActivityStartedEventDetails,

    -- * Lenses
    asedWorkerName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StepFunctions.Types.WorkerName as Types

-- | Contains details about the start of an activity during an execution.
--
-- /See:/ 'mkActivityStartedEventDetails' smart constructor.
newtype ActivityStartedEventDetails = ActivityStartedEventDetails'
  { -- | The name of the worker that the task is assigned to. These names are provided by the workers when calling 'GetActivityTask' .
    workerName :: Core.Maybe Types.WorkerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ActivityStartedEventDetails' value with any optional fields omitted.
mkActivityStartedEventDetails ::
  ActivityStartedEventDetails
mkActivityStartedEventDetails =
  ActivityStartedEventDetails' {workerName = Core.Nothing}

-- | The name of the worker that the task is assigned to. These names are provided by the workers when calling 'GetActivityTask' .
--
-- /Note:/ Consider using 'workerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asedWorkerName :: Lens.Lens' ActivityStartedEventDetails (Core.Maybe Types.WorkerName)
asedWorkerName = Lens.field @"workerName"
{-# DEPRECATED asedWorkerName "Use generic-lens or generic-optics with 'workerName' instead." #-}

instance Core.FromJSON ActivityStartedEventDetails where
  parseJSON =
    Core.withObject "ActivityStartedEventDetails" Core.$
      \x ->
        ActivityStartedEventDetails' Core.<$> (x Core..:? "workerName")
