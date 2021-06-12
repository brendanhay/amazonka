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
-- Module      : Network.AWS.CodeDeploy.Types.TagFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TagFilter where

import Network.AWS.CodeDeploy.Types.TagFilterType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an on-premises instance tag filter.
--
-- /See:/ 'newTagFilter' smart constructor.
data TagFilter = TagFilter'
  { -- | The on-premises instance tag filter key.
    key :: Core.Maybe Core.Text,
    -- | The on-premises instance tag filter value.
    value :: Core.Maybe Core.Text,
    -- | The on-premises instance tag filter type:
    --
    -- -   KEY_ONLY: Key only.
    --
    -- -   VALUE_ONLY: Value only.
    --
    -- -   KEY_AND_VALUE: Key and value.
    type' :: Core.Maybe TagFilterType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TagFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tagFilter_key' - The on-premises instance tag filter key.
--
-- 'value', 'tagFilter_value' - The on-premises instance tag filter value.
--
-- 'type'', 'tagFilter_type' - The on-premises instance tag filter type:
--
-- -   KEY_ONLY: Key only.
--
-- -   VALUE_ONLY: Value only.
--
-- -   KEY_AND_VALUE: Key and value.
newTagFilter ::
  TagFilter
newTagFilter =
  TagFilter'
    { key = Core.Nothing,
      value = Core.Nothing,
      type' = Core.Nothing
    }

-- | The on-premises instance tag filter key.
tagFilter_key :: Lens.Lens' TagFilter (Core.Maybe Core.Text)
tagFilter_key = Lens.lens (\TagFilter' {key} -> key) (\s@TagFilter' {} a -> s {key = a} :: TagFilter)

-- | The on-premises instance tag filter value.
tagFilter_value :: Lens.Lens' TagFilter (Core.Maybe Core.Text)
tagFilter_value = Lens.lens (\TagFilter' {value} -> value) (\s@TagFilter' {} a -> s {value = a} :: TagFilter)

-- | The on-premises instance tag filter type:
--
-- -   KEY_ONLY: Key only.
--
-- -   VALUE_ONLY: Value only.
--
-- -   KEY_AND_VALUE: Key and value.
tagFilter_type :: Lens.Lens' TagFilter (Core.Maybe TagFilterType)
tagFilter_type = Lens.lens (\TagFilter' {type'} -> type') (\s@TagFilter' {} a -> s {type' = a} :: TagFilter)

instance Core.FromJSON TagFilter where
  parseJSON =
    Core.withObject
      "TagFilter"
      ( \x ->
          TagFilter'
            Core.<$> (x Core..:? "Key")
            Core.<*> (x Core..:? "Value")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable TagFilter

instance Core.NFData TagFilter

instance Core.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Value" Core..=) Core.<$> value,
            ("Type" Core..=) Core.<$> type'
          ]
      )
