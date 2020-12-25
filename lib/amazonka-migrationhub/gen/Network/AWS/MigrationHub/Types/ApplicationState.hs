{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ApplicationState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ApplicationState
  ( ApplicationState (..),

    -- * Smart constructor
    mkApplicationState,

    -- * Lenses
    asApplicationId,
    asApplicationStatus,
    asLastUpdatedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types.ApplicationId as Types
import qualified Network.AWS.MigrationHub.Types.ApplicationStatus as Types
import qualified Network.AWS.Prelude as Core

-- | The state of an application discovered through Migration Hub import, the AWS Agentless Discovery Connector, or the AWS Application Discovery Agent.
--
-- /See:/ 'mkApplicationState' smart constructor.
data ApplicationState = ApplicationState'
  { -- | The configurationId from the Application Discovery Service that uniquely identifies an application.
    applicationId :: Core.Maybe Types.ApplicationId,
    -- | The current status of an application.
    applicationStatus :: Core.Maybe Types.ApplicationStatus,
    -- | The timestamp when the application status was last updated.
    lastUpdatedTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ApplicationState' value with any optional fields omitted.
mkApplicationState ::
  ApplicationState
mkApplicationState =
  ApplicationState'
    { applicationId = Core.Nothing,
      applicationStatus = Core.Nothing,
      lastUpdatedTime = Core.Nothing
    }

-- | The configurationId from the Application Discovery Service that uniquely identifies an application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationId :: Lens.Lens' ApplicationState (Core.Maybe Types.ApplicationId)
asApplicationId = Lens.field @"applicationId"
{-# DEPRECATED asApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current status of an application.
--
-- /Note:/ Consider using 'applicationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationStatus :: Lens.Lens' ApplicationState (Core.Maybe Types.ApplicationStatus)
asApplicationStatus = Lens.field @"applicationStatus"
{-# DEPRECATED asApplicationStatus "Use generic-lens or generic-optics with 'applicationStatus' instead." #-}

-- | The timestamp when the application status was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLastUpdatedTime :: Lens.Lens' ApplicationState (Core.Maybe Core.NominalDiffTime)
asLastUpdatedTime = Lens.field @"lastUpdatedTime"
{-# DEPRECATED asLastUpdatedTime "Use generic-lens or generic-optics with 'lastUpdatedTime' instead." #-}

instance Core.FromJSON ApplicationState where
  parseJSON =
    Core.withObject "ApplicationState" Core.$
      \x ->
        ApplicationState'
          Core.<$> (x Core..:? "ApplicationId")
          Core.<*> (x Core..:? "ApplicationStatus")
          Core.<*> (x Core..:? "LastUpdatedTime")
