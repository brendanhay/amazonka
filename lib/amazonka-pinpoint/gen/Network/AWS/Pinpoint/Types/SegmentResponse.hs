{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentResponse
  ( SegmentResponse (..),

    -- * Smart constructor
    mkSegmentResponse,

    -- * Lenses
    sLastModifiedDate,
    sSegmentGroups,
    sName,
    sVersion,
    sImportDefinition,
    sDimensions,
    sTags,
    sSegmentType,
    sCreationDate,
    sId,
    sARN,
    sApplicationId,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SegmentDimensions
import Network.AWS.Pinpoint.Types.SegmentGroupList
import Network.AWS.Pinpoint.Types.SegmentImportResource
import Network.AWS.Pinpoint.Types.SegmentType
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the configuration, dimension, and other settings for a segment.
--
-- /See:/ 'mkSegmentResponse' smart constructor.
data SegmentResponse = SegmentResponse'
  { lastModifiedDate ::
      Lude.Maybe Lude.Text,
    segmentGroups :: Lude.Maybe SegmentGroupList,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Int,
    importDefinition :: Lude.Maybe SegmentImportResource,
    dimensions :: Lude.Maybe SegmentDimensions,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    segmentType :: SegmentType,
    creationDate :: Lude.Text,
    id :: Lude.Text,
    arn :: Lude.Text,
    applicationId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the segment is associated with.
-- * 'arn' - The Amazon Resource Name (ARN) of the segment.
-- * 'creationDate' - The date and time when the segment was created.
-- * 'dimensions' - The dimension settings for the segment.
-- * 'id' - The unique identifier for the segment.
-- * 'importDefinition' - The settings for the import job that's associated with the segment.
-- * 'lastModifiedDate' - The date and time when the segment was last modified.
-- * 'name' - The name of the segment.
-- * 'segmentGroups' - A list of one or more segment groups that apply to the segment. Each segment group consists of zero or more base segments and the dimensions that are applied to those base segments.
-- * 'segmentType' - The segment type. Valid values are:
--
--
--     * DIMENSIONAL - A dynamic segment, which is a segment that uses selection criteria that you specify and is based on endpoint data that's reported by your app. Dynamic segments can change over time.
--
--
--     * IMPORT - A static segment, which is a segment that uses selection criteria that you specify and is based on endpoint definitions that you import from a file. Imported segments are static; they don't change over time.
--
--
-- * 'tags' - A string-to-string map of key-value pairs that identifies the tags that are associated with the segment. Each tag consists of a required tag key and an associated tag value.
-- * 'version' - The version number of the segment.
mkSegmentResponse ::
  -- | 'segmentType'
  SegmentType ->
  -- | 'creationDate'
  Lude.Text ->
  -- | 'id'
  Lude.Text ->
  -- | 'arn'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  SegmentResponse
mkSegmentResponse
  pSegmentType_
  pCreationDate_
  pId_
  pARN_
  pApplicationId_ =
    SegmentResponse'
      { lastModifiedDate = Lude.Nothing,
        segmentGroups = Lude.Nothing,
        name = Lude.Nothing,
        version = Lude.Nothing,
        importDefinition = Lude.Nothing,
        dimensions = Lude.Nothing,
        tags = Lude.Nothing,
        segmentType = pSegmentType_,
        creationDate = pCreationDate_,
        id = pId_,
        arn = pARN_,
        applicationId = pApplicationId_
      }

-- | The date and time when the segment was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLastModifiedDate :: Lens.Lens' SegmentResponse (Lude.Maybe Lude.Text)
sLastModifiedDate = Lens.lens (lastModifiedDate :: SegmentResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: SegmentResponse)
{-# DEPRECATED sLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | A list of one or more segment groups that apply to the segment. Each segment group consists of zero or more base segments and the dimensions that are applied to those base segments.
--
-- /Note:/ Consider using 'segmentGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSegmentGroups :: Lens.Lens' SegmentResponse (Lude.Maybe SegmentGroupList)
sSegmentGroups = Lens.lens (segmentGroups :: SegmentResponse -> Lude.Maybe SegmentGroupList) (\s a -> s {segmentGroups = a} :: SegmentResponse)
{-# DEPRECATED sSegmentGroups "Use generic-lens or generic-optics with 'segmentGroups' instead." #-}

-- | The name of the segment.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' SegmentResponse (Lude.Maybe Lude.Text)
sName = Lens.lens (name :: SegmentResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SegmentResponse)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version number of the segment.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sVersion :: Lens.Lens' SegmentResponse (Lude.Maybe Lude.Int)
sVersion = Lens.lens (version :: SegmentResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: SegmentResponse)
{-# DEPRECATED sVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The settings for the import job that's associated with the segment.
--
-- /Note:/ Consider using 'importDefinition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sImportDefinition :: Lens.Lens' SegmentResponse (Lude.Maybe SegmentImportResource)
sImportDefinition = Lens.lens (importDefinition :: SegmentResponse -> Lude.Maybe SegmentImportResource) (\s a -> s {importDefinition = a} :: SegmentResponse)
{-# DEPRECATED sImportDefinition "Use generic-lens or generic-optics with 'importDefinition' instead." #-}

-- | The dimension settings for the segment.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDimensions :: Lens.Lens' SegmentResponse (Lude.Maybe SegmentDimensions)
sDimensions = Lens.lens (dimensions :: SegmentResponse -> Lude.Maybe SegmentDimensions) (\s a -> s {dimensions = a} :: SegmentResponse)
{-# DEPRECATED sDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | A string-to-string map of key-value pairs that identifies the tags that are associated with the segment. Each tag consists of a required tag key and an associated tag value.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTags :: Lens.Lens' SegmentResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sTags = Lens.lens (tags :: SegmentResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: SegmentResponse)
{-# DEPRECATED sTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The segment type. Valid values are:
--
--
--     * DIMENSIONAL - A dynamic segment, which is a segment that uses selection criteria that you specify and is based on endpoint data that's reported by your app. Dynamic segments can change over time.
--
--
--     * IMPORT - A static segment, which is a segment that uses selection criteria that you specify and is based on endpoint definitions that you import from a file. Imported segments are static; they don't change over time.
--
--
--
-- /Note:/ Consider using 'segmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSegmentType :: Lens.Lens' SegmentResponse SegmentType
sSegmentType = Lens.lens (segmentType :: SegmentResponse -> SegmentType) (\s a -> s {segmentType = a} :: SegmentResponse)
{-# DEPRECATED sSegmentType "Use generic-lens or generic-optics with 'segmentType' instead." #-}

-- | The date and time when the segment was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCreationDate :: Lens.Lens' SegmentResponse Lude.Text
sCreationDate = Lens.lens (creationDate :: SegmentResponse -> Lude.Text) (\s a -> s {creationDate = a} :: SegmentResponse)
{-# DEPRECATED sCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The unique identifier for the segment.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sId :: Lens.Lens' SegmentResponse Lude.Text
sId = Lens.lens (id :: SegmentResponse -> Lude.Text) (\s a -> s {id = a} :: SegmentResponse)
{-# DEPRECATED sId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The Amazon Resource Name (ARN) of the segment.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sARN :: Lens.Lens' SegmentResponse Lude.Text
sARN = Lens.lens (arn :: SegmentResponse -> Lude.Text) (\s a -> s {arn = a} :: SegmentResponse)
{-# DEPRECATED sARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The unique identifier for the application that the segment is associated with.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sApplicationId :: Lens.Lens' SegmentResponse Lude.Text
sApplicationId = Lens.lens (applicationId :: SegmentResponse -> Lude.Text) (\s a -> s {applicationId = a} :: SegmentResponse)
{-# DEPRECATED sApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.FromJSON SegmentResponse where
  parseJSON =
    Lude.withObject
      "SegmentResponse"
      ( \x ->
          SegmentResponse'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "SegmentGroups")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "ImportDefinition")
            Lude.<*> (x Lude..:? "Dimensions")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "SegmentType")
            Lude.<*> (x Lude..: "CreationDate")
            Lude.<*> (x Lude..: "Id")
            Lude.<*> (x Lude..: "Arn")
            Lude.<*> (x Lude..: "ApplicationId")
      )
