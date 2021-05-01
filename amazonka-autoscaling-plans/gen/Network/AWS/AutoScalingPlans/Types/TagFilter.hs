{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AutoScalingPlans.Types.TagFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.TagFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a tag.
--
-- /See:/ 'newTagFilter' smart constructor.
data TagFilter = TagFilter'
  { -- | The tag key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The tag values (0 to 20).
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TagFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tagFilter_key' - The tag key.
--
-- 'values', 'tagFilter_values' - The tag values (0 to 20).
newTagFilter ::
  TagFilter
newTagFilter =
  TagFilter'
    { key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The tag key.
tagFilter_key :: Lens.Lens' TagFilter (Prelude.Maybe Prelude.Text)
tagFilter_key = Lens.lens (\TagFilter' {key} -> key) (\s@TagFilter' {} a -> s {key = a} :: TagFilter)

-- | The tag values (0 to 20).
tagFilter_values :: Lens.Lens' TagFilter (Prelude.Maybe [Prelude.Text])
tagFilter_values = Lens.lens (\TagFilter' {values} -> values) (\s@TagFilter' {} a -> s {values = a} :: TagFilter) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON TagFilter where
  parseJSON =
    Prelude.withObject
      "TagFilter"
      ( \x ->
          TagFilter'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Values" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable TagFilter

instance Prelude.NFData TagFilter

instance Prelude.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Values" Prelude..=) Prelude.<$> values
          ]
      )
