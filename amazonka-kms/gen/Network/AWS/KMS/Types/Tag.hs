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
-- Module      : Network.AWS.KMS.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A key-value pair. A tag consists of a tag key and a tag value. Tag keys
-- and tag values are both required, but tag values can be empty (null)
-- strings.
--
-- For information about the rules that apply to tag keys and tag values,
-- see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions>
-- in the /AWS Billing and Cost Management User Guide/.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The key of the tag.
    tagKey :: Core.Text,
    -- | The value of the tag.
    tagValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKey', 'tag_tagKey' - The key of the tag.
--
-- 'tagValue', 'tag_tagValue' - The value of the tag.
newTag ::
  -- | 'tagKey'
  Core.Text ->
  -- | 'tagValue'
  Core.Text ->
  Tag
newTag pTagKey_ pTagValue_ =
  Tag' {tagKey = pTagKey_, tagValue = pTagValue_}

-- | The key of the tag.
tag_tagKey :: Lens.Lens' Tag Core.Text
tag_tagKey = Lens.lens (\Tag' {tagKey} -> tagKey) (\s@Tag' {} a -> s {tagKey = a} :: Tag)

-- | The value of the tag.
tag_tagValue :: Lens.Lens' Tag Core.Text
tag_tagValue = Lens.lens (\Tag' {tagValue} -> tagValue) (\s@Tag' {} a -> s {tagValue = a} :: Tag)

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject
      "Tag"
      ( \x ->
          Tag'
            Core.<$> (x Core..: "TagKey") Core.<*> (x Core..: "TagValue")
      )

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TagKey" Core..= tagKey),
            Core.Just ("TagValue" Core..= tagValue)
          ]
      )
