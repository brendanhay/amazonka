{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Types.SegmentResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SegmentResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.SegmentDimensions
import Amazonka.Pinpoint.Types.SegmentGroupList
import Amazonka.Pinpoint.Types.SegmentImportResource
import Amazonka.Pinpoint.Types.SegmentType
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the configuration, dimension, and other
-- settings for a segment.
--
-- /See:/ 'newSegmentResponse' smart constructor.
data SegmentResponse = SegmentResponse'
  { -- | A string-to-string map of key-value pairs that identifies the tags that
    -- are associated with the segment. Each tag consists of a required tag key
    -- and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the segment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the segment was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The settings for the import job that\'s associated with the segment.
    importDefinition :: Prelude.Maybe SegmentImportResource,
    -- | The dimension settings for the segment.
    dimensions :: Prelude.Maybe SegmentDimensions,
    -- | A list of one or more segment groups that apply to the segment. Each
    -- segment group consists of zero or more base segments and the dimensions
    -- that are applied to those base segments.
    segmentGroups :: Prelude.Maybe SegmentGroupList,
    -- | The version number of the segment.
    version :: Prelude.Maybe Prelude.Int,
    -- | The segment type. Valid values are:
    --
    -- -   DIMENSIONAL - A dynamic segment, which is a segment that uses
    --     selection criteria that you specify and is based on endpoint data
    --     that\'s reported by your app. Dynamic segments can change over time.
    --
    -- -   IMPORT - A static segment, which is a segment that uses selection
    --     criteria that you specify and is based on endpoint definitions that
    --     you import from a file. Imported segments are static; they don\'t
    --     change over time.
    segmentType :: SegmentType,
    -- | The date and time when the segment was created.
    creationDate :: Prelude.Text,
    -- | The unique identifier for the segment.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the segment.
    arn :: Prelude.Text,
    -- | The unique identifier for the application that the segment is associated
    -- with.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'segmentResponse_tags' - A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the segment. Each tag consists of a required tag key
-- and an associated tag value.
--
-- 'name', 'segmentResponse_name' - The name of the segment.
--
-- 'lastModifiedDate', 'segmentResponse_lastModifiedDate' - The date and time when the segment was last modified.
--
-- 'importDefinition', 'segmentResponse_importDefinition' - The settings for the import job that\'s associated with the segment.
--
-- 'dimensions', 'segmentResponse_dimensions' - The dimension settings for the segment.
--
-- 'segmentGroups', 'segmentResponse_segmentGroups' - A list of one or more segment groups that apply to the segment. Each
-- segment group consists of zero or more base segments and the dimensions
-- that are applied to those base segments.
--
-- 'version', 'segmentResponse_version' - The version number of the segment.
--
-- 'segmentType', 'segmentResponse_segmentType' - The segment type. Valid values are:
--
-- -   DIMENSIONAL - A dynamic segment, which is a segment that uses
--     selection criteria that you specify and is based on endpoint data
--     that\'s reported by your app. Dynamic segments can change over time.
--
-- -   IMPORT - A static segment, which is a segment that uses selection
--     criteria that you specify and is based on endpoint definitions that
--     you import from a file. Imported segments are static; they don\'t
--     change over time.
--
-- 'creationDate', 'segmentResponse_creationDate' - The date and time when the segment was created.
--
-- 'id', 'segmentResponse_id' - The unique identifier for the segment.
--
-- 'arn', 'segmentResponse_arn' - The Amazon Resource Name (ARN) of the segment.
--
-- 'applicationId', 'segmentResponse_applicationId' - The unique identifier for the application that the segment is associated
-- with.
newSegmentResponse ::
  -- | 'segmentType'
  SegmentType ->
  -- | 'creationDate'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  SegmentResponse
newSegmentResponse
  pSegmentType_
  pCreationDate_
  pId_
  pArn_
  pApplicationId_ =
    SegmentResponse'
      { tags = Prelude.Nothing,
        name = Prelude.Nothing,
        lastModifiedDate = Prelude.Nothing,
        importDefinition = Prelude.Nothing,
        dimensions = Prelude.Nothing,
        segmentGroups = Prelude.Nothing,
        version = Prelude.Nothing,
        segmentType = pSegmentType_,
        creationDate = pCreationDate_,
        id = pId_,
        arn = pArn_,
        applicationId = pApplicationId_
      }

-- | A string-to-string map of key-value pairs that identifies the tags that
-- are associated with the segment. Each tag consists of a required tag key
-- and an associated tag value.
segmentResponse_tags :: Lens.Lens' SegmentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
segmentResponse_tags = Lens.lens (\SegmentResponse' {tags} -> tags) (\s@SegmentResponse' {} a -> s {tags = a} :: SegmentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the segment.
segmentResponse_name :: Lens.Lens' SegmentResponse (Prelude.Maybe Prelude.Text)
segmentResponse_name = Lens.lens (\SegmentResponse' {name} -> name) (\s@SegmentResponse' {} a -> s {name = a} :: SegmentResponse)

-- | The date and time when the segment was last modified.
segmentResponse_lastModifiedDate :: Lens.Lens' SegmentResponse (Prelude.Maybe Prelude.Text)
segmentResponse_lastModifiedDate = Lens.lens (\SegmentResponse' {lastModifiedDate} -> lastModifiedDate) (\s@SegmentResponse' {} a -> s {lastModifiedDate = a} :: SegmentResponse)

-- | The settings for the import job that\'s associated with the segment.
segmentResponse_importDefinition :: Lens.Lens' SegmentResponse (Prelude.Maybe SegmentImportResource)
segmentResponse_importDefinition = Lens.lens (\SegmentResponse' {importDefinition} -> importDefinition) (\s@SegmentResponse' {} a -> s {importDefinition = a} :: SegmentResponse)

-- | The dimension settings for the segment.
segmentResponse_dimensions :: Lens.Lens' SegmentResponse (Prelude.Maybe SegmentDimensions)
segmentResponse_dimensions = Lens.lens (\SegmentResponse' {dimensions} -> dimensions) (\s@SegmentResponse' {} a -> s {dimensions = a} :: SegmentResponse)

-- | A list of one or more segment groups that apply to the segment. Each
-- segment group consists of zero or more base segments and the dimensions
-- that are applied to those base segments.
segmentResponse_segmentGroups :: Lens.Lens' SegmentResponse (Prelude.Maybe SegmentGroupList)
segmentResponse_segmentGroups = Lens.lens (\SegmentResponse' {segmentGroups} -> segmentGroups) (\s@SegmentResponse' {} a -> s {segmentGroups = a} :: SegmentResponse)

-- | The version number of the segment.
segmentResponse_version :: Lens.Lens' SegmentResponse (Prelude.Maybe Prelude.Int)
segmentResponse_version = Lens.lens (\SegmentResponse' {version} -> version) (\s@SegmentResponse' {} a -> s {version = a} :: SegmentResponse)

-- | The segment type. Valid values are:
--
-- -   DIMENSIONAL - A dynamic segment, which is a segment that uses
--     selection criteria that you specify and is based on endpoint data
--     that\'s reported by your app. Dynamic segments can change over time.
--
-- -   IMPORT - A static segment, which is a segment that uses selection
--     criteria that you specify and is based on endpoint definitions that
--     you import from a file. Imported segments are static; they don\'t
--     change over time.
segmentResponse_segmentType :: Lens.Lens' SegmentResponse SegmentType
segmentResponse_segmentType = Lens.lens (\SegmentResponse' {segmentType} -> segmentType) (\s@SegmentResponse' {} a -> s {segmentType = a} :: SegmentResponse)

-- | The date and time when the segment was created.
segmentResponse_creationDate :: Lens.Lens' SegmentResponse Prelude.Text
segmentResponse_creationDate = Lens.lens (\SegmentResponse' {creationDate} -> creationDate) (\s@SegmentResponse' {} a -> s {creationDate = a} :: SegmentResponse)

-- | The unique identifier for the segment.
segmentResponse_id :: Lens.Lens' SegmentResponse Prelude.Text
segmentResponse_id = Lens.lens (\SegmentResponse' {id} -> id) (\s@SegmentResponse' {} a -> s {id = a} :: SegmentResponse)

-- | The Amazon Resource Name (ARN) of the segment.
segmentResponse_arn :: Lens.Lens' SegmentResponse Prelude.Text
segmentResponse_arn = Lens.lens (\SegmentResponse' {arn} -> arn) (\s@SegmentResponse' {} a -> s {arn = a} :: SegmentResponse)

-- | The unique identifier for the application that the segment is associated
-- with.
segmentResponse_applicationId :: Lens.Lens' SegmentResponse Prelude.Text
segmentResponse_applicationId = Lens.lens (\SegmentResponse' {applicationId} -> applicationId) (\s@SegmentResponse' {} a -> s {applicationId = a} :: SegmentResponse)

instance Core.FromJSON SegmentResponse where
  parseJSON =
    Core.withObject
      "SegmentResponse"
      ( \x ->
          SegmentResponse'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "ImportDefinition")
            Prelude.<*> (x Core..:? "Dimensions")
            Prelude.<*> (x Core..:? "SegmentGroups")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..: "SegmentType")
            Prelude.<*> (x Core..: "CreationDate")
            Prelude.<*> (x Core..: "Id")
            Prelude.<*> (x Core..: "Arn")
            Prelude.<*> (x Core..: "ApplicationId")
      )

instance Prelude.Hashable SegmentResponse where
  hashWithSalt _salt SegmentResponse' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` importDefinition
      `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` segmentGroups
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` segmentType
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData SegmentResponse where
  rnf SegmentResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf importDefinition
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf segmentGroups
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf segmentType
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf applicationId
