{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.ProjectSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Mobile.Types.ProjectSummary
  ( ProjectSummary (..)
  -- * Smart constructor
  , mkProjectSummary
  -- * Lenses
  , psName
  , psProjectId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Mobile.Types.ProjectId as Types
import qualified Network.AWS.Mobile.Types.ProjectName as Types
import qualified Network.AWS.Prelude as Core

-- | Summary information about an AWS Mobile Hub project. 
--
-- /See:/ 'mkProjectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { name :: Core.Maybe Types.ProjectName
    -- ^ Name of the project. 
  , projectId :: Core.Maybe Types.ProjectId
    -- ^ Unique project identifier. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectSummary' value with any optional fields omitted.
mkProjectSummary
    :: ProjectSummary
mkProjectSummary
  = ProjectSummary'{name = Core.Nothing, projectId = Core.Nothing}

-- | Name of the project. 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' ProjectSummary (Core.Maybe Types.ProjectName)
psName = Lens.field @"name"
{-# INLINEABLE psName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Unique project identifier. 
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psProjectId :: Lens.Lens' ProjectSummary (Core.Maybe Types.ProjectId)
psProjectId = Lens.field @"projectId"
{-# INLINEABLE psProjectId #-}
{-# DEPRECATED projectId "Use generic-lens or generic-optics with 'projectId' instead"  #-}

instance Core.FromJSON ProjectSummary where
        parseJSON
          = Core.withObject "ProjectSummary" Core.$
              \ x ->
                ProjectSummary' Core.<$>
                  (x Core..:? "name") Core.<*> x Core..:? "projectId"
