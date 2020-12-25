{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroup
  ( ReportGroup (..),

    -- * Smart constructor
    mkReportGroup,

    -- * Lenses
    rgArn,
    rgCreated,
    rgExportConfig,
    rgLastModified,
    rgName,
    rgStatus,
    rgTags,
    rgType,
  )
where

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
  { -- | The ARN of a @ReportGroup@ .
    arn :: Core.Maybe Types.NonEmptyString,
    -- | The date and time this @ReportGroup@ was created.
    created :: Core.Maybe Core.NominalDiffTime,
    -- | Information about the destination where the raw data of this @ReportGroup@ is exported.
    exportConfig :: Core.Maybe Types.ReportExportConfig,
    -- | The date and time this @ReportGroup@ was last modified.
    lastModified :: Core.Maybe Core.NominalDiffTime,
    -- | The name of a @ReportGroup@ .
    name :: Core.Maybe Types.ReportGroupName,
    status :: Core.Maybe Types.ReportGroupStatusType,
    -- | A list of tag key and value pairs associated with this report group.
    --
    -- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
    tags :: Core.Maybe [Types.Tag],
    -- | The type of the @ReportGroup@ . The one valid value is @TEST@ .
    type' :: Core.Maybe Types.ReportType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReportGroup' value with any optional fields omitted.
mkReportGroup ::
  ReportGroup
mkReportGroup =
  ReportGroup'
    { arn = Core.Nothing,
      created = Core.Nothing,
      exportConfig = Core.Nothing,
      lastModified = Core.Nothing,
      name = Core.Nothing,
      status = Core.Nothing,
      tags = Core.Nothing,
      type' = Core.Nothing
    }

-- | The ARN of a @ReportGroup@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgArn :: Lens.Lens' ReportGroup (Core.Maybe Types.NonEmptyString)
rgArn = Lens.field @"arn"
{-# DEPRECATED rgArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time this @ReportGroup@ was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgCreated :: Lens.Lens' ReportGroup (Core.Maybe Core.NominalDiffTime)
rgCreated = Lens.field @"created"
{-# DEPRECATED rgCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | Information about the destination where the raw data of this @ReportGroup@ is exported.
--
-- /Note:/ Consider using 'exportConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgExportConfig :: Lens.Lens' ReportGroup (Core.Maybe Types.ReportExportConfig)
rgExportConfig = Lens.field @"exportConfig"
{-# DEPRECATED rgExportConfig "Use generic-lens or generic-optics with 'exportConfig' instead." #-}

-- | The date and time this @ReportGroup@ was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgLastModified :: Lens.Lens' ReportGroup (Core.Maybe Core.NominalDiffTime)
rgLastModified = Lens.field @"lastModified"
{-# DEPRECATED rgLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | The name of a @ReportGroup@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgName :: Lens.Lens' ReportGroup (Core.Maybe Types.ReportGroupName)
rgName = Lens.field @"name"
{-# DEPRECATED rgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgStatus :: Lens.Lens' ReportGroup (Core.Maybe Types.ReportGroupStatusType)
rgStatus = Lens.field @"status"
{-# DEPRECATED rgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgTags :: Lens.Lens' ReportGroup (Core.Maybe [Types.Tag])
rgTags = Lens.field @"tags"
{-# DEPRECATED rgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The type of the @ReportGroup@ . The one valid value is @TEST@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgType :: Lens.Lens' ReportGroup (Core.Maybe Types.ReportType)
rgType = Lens.field @"type'"
{-# DEPRECATED rgType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON ReportGroup where
  parseJSON =
    Core.withObject "ReportGroup" Core.$
      \x ->
        ReportGroup'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "created")
          Core.<*> (x Core..:? "exportConfig")
          Core.<*> (x Core..:? "lastModified")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "tags")
          Core.<*> (x Core..:? "type")
