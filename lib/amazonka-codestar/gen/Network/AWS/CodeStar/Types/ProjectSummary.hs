{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.ProjectSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeStar.Types.ProjectSummary
  ( ProjectSummary (..)
  -- * Smart constructor
  , mkProjectSummary
  -- * Lenses
  , psProjectArn
  , psProjectId
  ) where

import qualified Network.AWS.CodeStar.Types.ProjectArn as Types
import qualified Network.AWS.CodeStar.Types.ProjectId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the metadata for a project.
--
-- /See:/ 'mkProjectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { projectArn :: Core.Maybe Types.ProjectArn
    -- ^ The Amazon Resource Name (ARN) of the project.
  , projectId :: Core.Maybe Types.ProjectId
    -- ^ The ID of the project.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProjectSummary' value with any optional fields omitted.
mkProjectSummary
    :: ProjectSummary
mkProjectSummary
  = ProjectSummary'{projectArn = Core.Nothing,
                    projectId = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the project.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psProjectArn :: Lens.Lens' ProjectSummary (Core.Maybe Types.ProjectArn)
psProjectArn = Lens.field @"projectArn"
{-# INLINEABLE psProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | The ID of the project.
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
                  (x Core..:? "projectArn") Core.<*> x Core..:? "projectId"
