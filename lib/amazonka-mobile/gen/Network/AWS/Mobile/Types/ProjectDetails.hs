{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.ProjectDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Mobile.Types.ProjectDetails
  ( ProjectDetails (..)
  -- * Smart constructor
  , mkProjectDetails
  -- * Lenses
  , pdConsoleUrl
  , pdCreatedDate
  , pdLastUpdatedDate
  , pdName
  , pdProjectId
  , pdRegion
  , pdResources
  , pdState
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types.ConsoleUrl as Types
import qualified Network.AWS.Mobile.Types.Name as Types
import qualified Network.AWS.Mobile.Types.ProjectId as Types
import qualified Network.AWS.Mobile.Types.ProjectState as Types
import qualified Network.AWS.Mobile.Types.Region as Types
import qualified Network.AWS.Mobile.Types.Resource as Types
import qualified Network.AWS.Prelude as Core

-- | Detailed information about an AWS Mobile Hub project. 
--
-- /See:/ 'mkProjectDetails' smart constructor.
data ProjectDetails = ProjectDetails'
  { consoleUrl :: Core.Maybe Types.ConsoleUrl
    -- ^ Website URL for this project in the AWS Mobile Hub console. 
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ Date the project was created. 
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ Date of the last modification of the project. 
  , name :: Core.Maybe Types.Name
  , projectId :: Core.Maybe Types.ProjectId
  , region :: Core.Maybe Types.Region
  , resources :: Core.Maybe [Types.Resource]
  , state :: Core.Maybe Types.ProjectState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProjectDetails' value with any optional fields omitted.
mkProjectDetails
    :: ProjectDetails
mkProjectDetails
  = ProjectDetails'{consoleUrl = Core.Nothing,
                    createdDate = Core.Nothing, lastUpdatedDate = Core.Nothing,
                    name = Core.Nothing, projectId = Core.Nothing,
                    region = Core.Nothing, resources = Core.Nothing,
                    state = Core.Nothing}

-- | Website URL for this project in the AWS Mobile Hub console. 
--
-- /Note:/ Consider using 'consoleUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdConsoleUrl :: Lens.Lens' ProjectDetails (Core.Maybe Types.ConsoleUrl)
pdConsoleUrl = Lens.field @"consoleUrl"
{-# INLINEABLE pdConsoleUrl #-}
{-# DEPRECATED consoleUrl "Use generic-lens or generic-optics with 'consoleUrl' instead"  #-}

-- | Date the project was created. 
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCreatedDate :: Lens.Lens' ProjectDetails (Core.Maybe Core.NominalDiffTime)
pdCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE pdCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | Date of the last modification of the project. 
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdLastUpdatedDate :: Lens.Lens' ProjectDetails (Core.Maybe Core.NominalDiffTime)
pdLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE pdLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' ProjectDetails (Core.Maybe Types.Name)
pdName = Lens.field @"name"
{-# INLINEABLE pdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProjectId :: Lens.Lens' ProjectDetails (Core.Maybe Types.ProjectId)
pdProjectId = Lens.field @"projectId"
{-# INLINEABLE pdProjectId #-}
{-# DEPRECATED projectId "Use generic-lens or generic-optics with 'projectId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdRegion :: Lens.Lens' ProjectDetails (Core.Maybe Types.Region)
pdRegion = Lens.field @"region"
{-# INLINEABLE pdRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdResources :: Lens.Lens' ProjectDetails (Core.Maybe [Types.Resource])
pdResources = Lens.field @"resources"
{-# INLINEABLE pdResources #-}
{-# DEPRECATED resources "Use generic-lens or generic-optics with 'resources' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdState :: Lens.Lens' ProjectDetails (Core.Maybe Types.ProjectState)
pdState = Lens.field @"state"
{-# INLINEABLE pdState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON ProjectDetails where
        parseJSON
          = Core.withObject "ProjectDetails" Core.$
              \ x ->
                ProjectDetails' Core.<$>
                  (x Core..:? "consoleUrl") Core.<*> x Core..:? "createdDate"
                    Core.<*> x Core..:? "lastUpdatedDate"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "projectId"
                    Core.<*> x Core..:? "region"
                    Core.<*> x Core..:? "resources"
                    Core.<*> x Core..:? "state"
