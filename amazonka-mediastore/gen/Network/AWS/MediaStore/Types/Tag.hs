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
-- Module      : Network.AWS.MediaStore.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaStore.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A collection of tags associated with a container. Each tag consists of a
-- key:value pair, which can be anything you define. Typically, the tag key
-- represents a category (such as \"environment\") and the tag value
-- represents a specific value within that category (such as \"test,\"
-- \"development,\" or \"production\"). You can add up to 50 tags to each
-- container. For more information about tagging, including naming and
-- usage conventions, see
-- <https://docs.aws.amazon.com/mediastore/latest/ug/tagging.html Tagging Resources in MediaStore>.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | Part of the key:value pair that defines a tag. You can use a tag value
    -- to describe a specific value within a category, such as \"companyA\" or
    -- \"companyB.\" Tag values are case-sensitive.
    value :: Core.Maybe Core.Text,
    -- | Part of the key:value pair that defines a tag. You can use a tag key to
    -- describe a category of information, such as \"customer.\" Tag keys are
    -- case-sensitive.
    key :: Core.Text
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
-- 'value', 'tag_value' - Part of the key:value pair that defines a tag. You can use a tag value
-- to describe a specific value within a category, such as \"companyA\" or
-- \"companyB.\" Tag values are case-sensitive.
--
-- 'key', 'tag_key' - Part of the key:value pair that defines a tag. You can use a tag key to
-- describe a category of information, such as \"customer.\" Tag keys are
-- case-sensitive.
newTag ::
  -- | 'key'
  Core.Text ->
  Tag
newTag pKey_ =
  Tag' {value = Core.Nothing, key = pKey_}

-- | Part of the key:value pair that defines a tag. You can use a tag value
-- to describe a specific value within a category, such as \"companyA\" or
-- \"companyB.\" Tag values are case-sensitive.
tag_value :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | Part of the key:value pair that defines a tag. You can use a tag key to
-- describe a category of information, such as \"customer.\" Tag keys are
-- case-sensitive.
tag_key :: Lens.Lens' Tag Core.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject
      "Tag"
      ( \x ->
          Tag'
            Core.<$> (x Core..:? "Value") Core.<*> (x Core..: "Key")
      )

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Value" Core..=) Core.<$> value,
            Core.Just ("Key" Core..= key)
          ]
      )
