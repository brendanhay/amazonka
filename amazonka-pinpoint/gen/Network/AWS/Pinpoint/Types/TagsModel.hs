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
-- Module      : Network.AWS.Pinpoint.Types.TagsModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TagsModel where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the tags (keys and values) for an application, campaign,
-- message template, or segment.
--
-- /See:/ 'newTagsModel' smart constructor.
data TagsModel = TagsModel'
  { -- | A string-to-string map of key-value pairs that defines the tags for an
    -- application, campaign, message template, or segment. Each of these
    -- resources can have a maximum of 50 tags.
    --
    -- Each tag consists of a required tag key and an associated tag value. The
    -- maximum length of a tag key is 128 characters. The maximum length of a
    -- tag value is 256 characters.
    tags :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagsModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'tagsModel_tags' - A string-to-string map of key-value pairs that defines the tags for an
-- application, campaign, message template, or segment. Each of these
-- resources can have a maximum of 50 tags.
--
-- Each tag consists of a required tag key and an associated tag value. The
-- maximum length of a tag key is 128 characters. The maximum length of a
-- tag value is 256 characters.
newTagsModel ::
  TagsModel
newTagsModel = TagsModel' {tags = Core.mempty}

-- | A string-to-string map of key-value pairs that defines the tags for an
-- application, campaign, message template, or segment. Each of these
-- resources can have a maximum of 50 tags.
--
-- Each tag consists of a required tag key and an associated tag value. The
-- maximum length of a tag key is 128 characters. The maximum length of a
-- tag value is 256 characters.
tagsModel_tags :: Lens.Lens' TagsModel (Core.HashMap Core.Text Core.Text)
tagsModel_tags = Lens.lens (\TagsModel' {tags} -> tags) (\s@TagsModel' {} a -> s {tags = a} :: TagsModel) Core.. Lens._Coerce

instance Core.FromJSON TagsModel where
  parseJSON =
    Core.withObject
      "TagsModel"
      ( \x ->
          TagsModel'
            Core.<$> (x Core..:? "tags" Core..!= Core.mempty)
      )

instance Core.Hashable TagsModel

instance Core.NFData TagsModel

instance Core.ToJSON TagsModel where
  toJSON TagsModel' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("tags" Core..= tags)])
