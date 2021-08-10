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
import qualified Network.AWS.Prelude as Prelude

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
    tags :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
newTagsModel = TagsModel' {tags = Prelude.mempty}

-- | A string-to-string map of key-value pairs that defines the tags for an
-- application, campaign, message template, or segment. Each of these
-- resources can have a maximum of 50 tags.
--
-- Each tag consists of a required tag key and an associated tag value. The
-- maximum length of a tag key is 128 characters. The maximum length of a
-- tag value is 256 characters.
tagsModel_tags :: Lens.Lens' TagsModel (Prelude.HashMap Prelude.Text Prelude.Text)
tagsModel_tags = Lens.lens (\TagsModel' {tags} -> tags) (\s@TagsModel' {} a -> s {tags = a} :: TagsModel) Prelude.. Lens._Coerce

instance Core.FromJSON TagsModel where
  parseJSON =
    Core.withObject
      "TagsModel"
      ( \x ->
          TagsModel'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TagsModel

instance Prelude.NFData TagsModel

instance Core.ToJSON TagsModel where
  toJSON TagsModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("tags" Core..= tags)]
      )
