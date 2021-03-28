{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.ProjectDescription
  ( ProjectDescription (..)
  -- * Smart constructor
  , mkProjectDescription
  -- * Lenses
  , pdCreationTimestamp
  , pdProjectArn
  , pdStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.ProjectArn as Types
import qualified Network.AWS.Rekognition.Types.ProjectStatus as Types

-- | A description of a Amazon Rekognition Custom Labels project.
--
-- /See:/ 'mkProjectDescription' smart constructor.
data ProjectDescription = ProjectDescription'
  { creationTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The Unix timestamp for the date and time that the project was created.
  , projectArn :: Core.Maybe Types.ProjectArn
    -- ^ The Amazon Resource Name (ARN) of the project.
  , status :: Core.Maybe Types.ProjectStatus
    -- ^ The current status of the project.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ProjectDescription' value with any optional fields omitted.
mkProjectDescription
    :: ProjectDescription
mkProjectDescription
  = ProjectDescription'{creationTimestamp = Core.Nothing,
                        projectArn = Core.Nothing, status = Core.Nothing}

-- | The Unix timestamp for the date and time that the project was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCreationTimestamp :: Lens.Lens' ProjectDescription (Core.Maybe Core.NominalDiffTime)
pdCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE pdCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | The Amazon Resource Name (ARN) of the project.
--
-- /Note:/ Consider using 'projectArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProjectArn :: Lens.Lens' ProjectDescription (Core.Maybe Types.ProjectArn)
pdProjectArn = Lens.field @"projectArn"
{-# INLINEABLE pdProjectArn #-}
{-# DEPRECATED projectArn "Use generic-lens or generic-optics with 'projectArn' instead"  #-}

-- | The current status of the project.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdStatus :: Lens.Lens' ProjectDescription (Core.Maybe Types.ProjectStatus)
pdStatus = Lens.field @"status"
{-# INLINEABLE pdStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON ProjectDescription where
        parseJSON
          = Core.withObject "ProjectDescription" Core.$
              \ x ->
                ProjectDescription' Core.<$>
                  (x Core..:? "CreationTimestamp") Core.<*> x Core..:? "ProjectArn"
                    Core.<*> x Core..:? "Status"
