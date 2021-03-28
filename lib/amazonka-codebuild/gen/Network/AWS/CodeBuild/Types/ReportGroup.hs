{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ReportGroup
  ( ReportGroup (..)
  -- * Smart constructor
  , mkReportGroup
  -- * Lenses
  , rgArn
  , rgCreated
  , rgExportConfig
  , rgLastModified
  , rgName
  , rgStatus
  , rgTags
  , rgType
  ) where

import qualified Network.AWS.CodeBuild.Types.NonEmptyString as Types
import qualified Network.AWS.CodeBuild.Types.ReportExportConfig as Types
import qualified Network.AWS.CodeBuild.Types.ReportGroupName as Types
import qualified Network.AWS.CodeBuild.Types.ReportGroupStatusType as Types
import qualified Network.AWS.CodeBuild.Types.ReportType as Types
import qualified Network.AWS.CodeBuild.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A series of reports. Each report contains information about the results from running a series of test cases. You specify the test cases for a report group in the buildspec for a build project using one or more paths to the test case files. 
--
-- /See:/ 'mkReportGroup' smart constructor.
data ReportGroup = ReportGroup'
  { arn :: Core.Maybe Types.NonEmptyString
    -- ^ The ARN of a @ReportGroup@ . 
  , created :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time this @ReportGroup@ was created. 
  , exportConfig :: Core.Maybe Types.ReportExportConfig
    -- ^ Information about the destination where the raw data of this @ReportGroup@ is exported. 
  , lastModified :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time this @ReportGroup@ was last modified. 
  , name :: Core.Maybe Types.ReportGroupName
    -- ^ The name of a @ReportGroup@ . 
  , status :: Core.Maybe Types.ReportGroupStatusType
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tag key and value pairs associated with this report group. 
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
  , type' :: Core.Maybe Types.ReportType
    -- ^ The type of the @ReportGroup@ . The one valid value is @TEST@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ReportGroup' value with any optional fields omitted.
mkReportGroup
    :: ReportGroup
mkReportGroup
  = ReportGroup'{arn = Core.Nothing, created = Core.Nothing,
                 exportConfig = Core.Nothing, lastModified = Core.Nothing,
                 name = Core.Nothing, status = Core.Nothing, tags = Core.Nothing,
                 type' = Core.Nothing}

-- | The ARN of a @ReportGroup@ . 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgArn :: Lens.Lens' ReportGroup (Core.Maybe Types.NonEmptyString)
rgArn = Lens.field @"arn"
{-# INLINEABLE rgArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The date and time this @ReportGroup@ was created. 
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgCreated :: Lens.Lens' ReportGroup (Core.Maybe Core.NominalDiffTime)
rgCreated = Lens.field @"created"
{-# INLINEABLE rgCreated #-}
{-# DEPRECATED created "Use generic-lens or generic-optics with 'created' instead"  #-}

-- | Information about the destination where the raw data of this @ReportGroup@ is exported. 
--
-- /Note:/ Consider using 'exportConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgExportConfig :: Lens.Lens' ReportGroup (Core.Maybe Types.ReportExportConfig)
rgExportConfig = Lens.field @"exportConfig"
{-# INLINEABLE rgExportConfig #-}
{-# DEPRECATED exportConfig "Use generic-lens or generic-optics with 'exportConfig' instead"  #-}

-- | The date and time this @ReportGroup@ was last modified. 
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgLastModified :: Lens.Lens' ReportGroup (Core.Maybe Core.NominalDiffTime)
rgLastModified = Lens.field @"lastModified"
{-# INLINEABLE rgLastModified #-}
{-# DEPRECATED lastModified "Use generic-lens or generic-optics with 'lastModified' instead"  #-}

-- | The name of a @ReportGroup@ . 
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgName :: Lens.Lens' ReportGroup (Core.Maybe Types.ReportGroupName)
rgName = Lens.field @"name"
{-# INLINEABLE rgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgStatus :: Lens.Lens' ReportGroup (Core.Maybe Types.ReportGroupStatusType)
rgStatus = Lens.field @"status"
{-# INLINEABLE rgStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A list of tag key and value pairs associated with this report group. 
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgTags :: Lens.Lens' ReportGroup (Core.Maybe [Types.Tag])
rgTags = Lens.field @"tags"
{-# INLINEABLE rgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The type of the @ReportGroup@ . The one valid value is @TEST@ . 
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgType :: Lens.Lens' ReportGroup (Core.Maybe Types.ReportType)
rgType = Lens.field @"type'"
{-# INLINEABLE rgType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ReportGroup where
        parseJSON
          = Core.withObject "ReportGroup" Core.$
              \ x ->
                ReportGroup' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "created" Core.<*>
                    x Core..:? "exportConfig"
                    Core.<*> x Core..:? "lastModified"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "type"
