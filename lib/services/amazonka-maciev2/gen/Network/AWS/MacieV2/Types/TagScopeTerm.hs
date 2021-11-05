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
-- Module      : Network.AWS.MacieV2.Types.TagScopeTerm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.TagScopeTerm where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types.JobComparator
import Network.AWS.MacieV2.Types.TagTarget
import Network.AWS.MacieV2.Types.TagValuePair
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a tag-based condition that determines whether an S3 object is
-- included or excluded from a classification job.
--
-- /See:/ 'newTagScopeTerm' smart constructor.
data TagScopeTerm = TagScopeTerm'
  { -- | The tag keys or tag key and value pairs to use in the condition. To
    -- specify only tag keys in a condition, specify the keys in this array and
    -- set the value for each associated tag value to an empty string.
    tagValues :: Prelude.Maybe [TagValuePair],
    -- | The object property to use in the condition. The only valid value is
    -- TAG.
    key :: Prelude.Maybe Prelude.Text,
    -- | The operator to use in the condition. Valid values are EQ (equals) or NE
    -- (not equals).
    comparator :: Prelude.Maybe JobComparator,
    -- | The type of object to apply the condition to.
    target :: Prelude.Maybe TagTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagScopeTerm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagValues', 'tagScopeTerm_tagValues' - The tag keys or tag key and value pairs to use in the condition. To
-- specify only tag keys in a condition, specify the keys in this array and
-- set the value for each associated tag value to an empty string.
--
-- 'key', 'tagScopeTerm_key' - The object property to use in the condition. The only valid value is
-- TAG.
--
-- 'comparator', 'tagScopeTerm_comparator' - The operator to use in the condition. Valid values are EQ (equals) or NE
-- (not equals).
--
-- 'target', 'tagScopeTerm_target' - The type of object to apply the condition to.
newTagScopeTerm ::
  TagScopeTerm
newTagScopeTerm =
  TagScopeTerm'
    { tagValues = Prelude.Nothing,
      key = Prelude.Nothing,
      comparator = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | The tag keys or tag key and value pairs to use in the condition. To
-- specify only tag keys in a condition, specify the keys in this array and
-- set the value for each associated tag value to an empty string.
tagScopeTerm_tagValues :: Lens.Lens' TagScopeTerm (Prelude.Maybe [TagValuePair])
tagScopeTerm_tagValues = Lens.lens (\TagScopeTerm' {tagValues} -> tagValues) (\s@TagScopeTerm' {} a -> s {tagValues = a} :: TagScopeTerm) Prelude.. Lens.mapping Lens.coerced

-- | The object property to use in the condition. The only valid value is
-- TAG.
tagScopeTerm_key :: Lens.Lens' TagScopeTerm (Prelude.Maybe Prelude.Text)
tagScopeTerm_key = Lens.lens (\TagScopeTerm' {key} -> key) (\s@TagScopeTerm' {} a -> s {key = a} :: TagScopeTerm)

-- | The operator to use in the condition. Valid values are EQ (equals) or NE
-- (not equals).
tagScopeTerm_comparator :: Lens.Lens' TagScopeTerm (Prelude.Maybe JobComparator)
tagScopeTerm_comparator = Lens.lens (\TagScopeTerm' {comparator} -> comparator) (\s@TagScopeTerm' {} a -> s {comparator = a} :: TagScopeTerm)

-- | The type of object to apply the condition to.
tagScopeTerm_target :: Lens.Lens' TagScopeTerm (Prelude.Maybe TagTarget)
tagScopeTerm_target = Lens.lens (\TagScopeTerm' {target} -> target) (\s@TagScopeTerm' {} a -> s {target = a} :: TagScopeTerm)

instance Core.FromJSON TagScopeTerm where
  parseJSON =
    Core.withObject
      "TagScopeTerm"
      ( \x ->
          TagScopeTerm'
            Prelude.<$> (x Core..:? "tagValues" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "key")
            Prelude.<*> (x Core..:? "comparator")
            Prelude.<*> (x Core..:? "target")
      )

instance Prelude.Hashable TagScopeTerm

instance Prelude.NFData TagScopeTerm

instance Core.ToJSON TagScopeTerm where
  toJSON TagScopeTerm' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tagValues" Core..=) Prelude.<$> tagValues,
            ("key" Core..=) Prelude.<$> key,
            ("comparator" Core..=) Prelude.<$> comparator,
            ("target" Core..=) Prelude.<$> target
          ]
      )
