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
    rgStatus,
    rgArn,
    rgCreated,
    rgName,
    rgType,
    rgLastModified,
    rgExportConfig,
    rgTags,
  )
where

import Network.AWS.CodeBuild.Types.ReportExportConfig
import Network.AWS.CodeBuild.Types.ReportGroupStatusType
import Network.AWS.CodeBuild.Types.ReportType
import Network.AWS.CodeBuild.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A series of reports. Each report contains information about the results from running a series of test cases. You specify the test cases for a report group in the buildspec for a build project using one or more paths to the test case files.
--
-- /See:/ 'mkReportGroup' smart constructor.
data ReportGroup = ReportGroup'
  { status ::
      Lude.Maybe ReportGroupStatusType,
    arn :: Lude.Maybe Lude.Text,
    created :: Lude.Maybe Lude.Timestamp,
    name :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ReportType,
    lastModified :: Lude.Maybe Lude.Timestamp,
    exportConfig :: Lude.Maybe ReportExportConfig,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of a @ReportGroup@ .
-- * 'created' - The date and time this @ReportGroup@ was created.
-- * 'exportConfig' - Information about the destination where the raw data of this @ReportGroup@ is exported.
-- * 'lastModified' - The date and time this @ReportGroup@ was last modified.
-- * 'name' - The name of a @ReportGroup@ .
-- * 'status' - Undocumented field.
-- * 'tags' - A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
-- * 'type'' - The type of the @ReportGroup@ . The one valid value is @TEST@ .
mkReportGroup ::
  ReportGroup
mkReportGroup =
  ReportGroup'
    { status = Lude.Nothing,
      arn = Lude.Nothing,
      created = Lude.Nothing,
      name = Lude.Nothing,
      type' = Lude.Nothing,
      lastModified = Lude.Nothing,
      exportConfig = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgStatus :: Lens.Lens' ReportGroup (Lude.Maybe ReportGroupStatusType)
rgStatus = Lens.lens (status :: ReportGroup -> Lude.Maybe ReportGroupStatusType) (\s a -> s {status = a} :: ReportGroup)
{-# DEPRECATED rgStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of a @ReportGroup@ .
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgArn :: Lens.Lens' ReportGroup (Lude.Maybe Lude.Text)
rgArn = Lens.lens (arn :: ReportGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ReportGroup)
{-# DEPRECATED rgArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time this @ReportGroup@ was created.
--
-- /Note:/ Consider using 'created' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgCreated :: Lens.Lens' ReportGroup (Lude.Maybe Lude.Timestamp)
rgCreated = Lens.lens (created :: ReportGroup -> Lude.Maybe Lude.Timestamp) (\s a -> s {created = a} :: ReportGroup)
{-# DEPRECATED rgCreated "Use generic-lens or generic-optics with 'created' instead." #-}

-- | The name of a @ReportGroup@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgName :: Lens.Lens' ReportGroup (Lude.Maybe Lude.Text)
rgName = Lens.lens (name :: ReportGroup -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ReportGroup)
{-# DEPRECATED rgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the @ReportGroup@ . The one valid value is @TEST@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgType :: Lens.Lens' ReportGroup (Lude.Maybe ReportType)
rgType = Lens.lens (type' :: ReportGroup -> Lude.Maybe ReportType) (\s a -> s {type' = a} :: ReportGroup)
{-# DEPRECATED rgType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date and time this @ReportGroup@ was last modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgLastModified :: Lens.Lens' ReportGroup (Lude.Maybe Lude.Timestamp)
rgLastModified = Lens.lens (lastModified :: ReportGroup -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModified = a} :: ReportGroup)
{-# DEPRECATED rgLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | Information about the destination where the raw data of this @ReportGroup@ is exported.
--
-- /Note:/ Consider using 'exportConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgExportConfig :: Lens.Lens' ReportGroup (Lude.Maybe ReportExportConfig)
rgExportConfig = Lens.lens (exportConfig :: ReportGroup -> Lude.Maybe ReportExportConfig) (\s a -> s {exportConfig = a} :: ReportGroup)
{-# DEPRECATED rgExportConfig "Use generic-lens or generic-optics with 'exportConfig' instead." #-}

-- | A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS CodeBuild report group tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rgTags :: Lens.Lens' ReportGroup (Lude.Maybe [Tag])
rgTags = Lens.lens (tags :: ReportGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: ReportGroup)
{-# DEPRECATED rgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON ReportGroup where
  parseJSON =
    Lude.withObject
      "ReportGroup"
      ( \x ->
          ReportGroup'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "created")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "lastModified")
            Lude.<*> (x Lude..:? "exportConfig")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
