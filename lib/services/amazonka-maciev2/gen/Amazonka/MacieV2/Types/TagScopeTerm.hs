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
-- Module      : Amazonka.MacieV2.Types.TagScopeTerm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.TagScopeTerm where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.JobComparator
import Amazonka.MacieV2.Types.TagTarget
import Amazonka.MacieV2.Types.TagValuePair
import qualified Amazonka.Prelude as Prelude

-- | Specifies a tag-based condition that determines whether an S3 object is
-- included or excluded from a classification job.
--
-- /See:/ 'newTagScopeTerm' smart constructor.
data TagScopeTerm = TagScopeTerm'
  { -- | The object property to use in the condition. The only valid value is
    -- TAG.
    key :: Prelude.Maybe Prelude.Text,
    -- | The tag keys or tag key and value pairs to use in the condition. To
    -- specify only tag keys in a condition, specify the keys in this array and
    -- set the value for each associated tag value to an empty string.
    tagValues :: Prelude.Maybe [TagValuePair],
    -- | The type of object to apply the condition to.
    target :: Prelude.Maybe TagTarget,
    -- | The operator to use in the condition. Valid values are EQ (equals) or NE
    -- (not equals).
    comparator :: Prelude.Maybe JobComparator
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
-- 'key', 'tagScopeTerm_key' - The object property to use in the condition. The only valid value is
-- TAG.
--
-- 'tagValues', 'tagScopeTerm_tagValues' - The tag keys or tag key and value pairs to use in the condition. To
-- specify only tag keys in a condition, specify the keys in this array and
-- set the value for each associated tag value to an empty string.
--
-- 'target', 'tagScopeTerm_target' - The type of object to apply the condition to.
--
-- 'comparator', 'tagScopeTerm_comparator' - The operator to use in the condition. Valid values are EQ (equals) or NE
-- (not equals).
newTagScopeTerm ::
  TagScopeTerm
newTagScopeTerm =
  TagScopeTerm'
    { key = Prelude.Nothing,
      tagValues = Prelude.Nothing,
      target = Prelude.Nothing,
      comparator = Prelude.Nothing
    }

-- | The object property to use in the condition. The only valid value is
-- TAG.
tagScopeTerm_key :: Lens.Lens' TagScopeTerm (Prelude.Maybe Prelude.Text)
tagScopeTerm_key = Lens.lens (\TagScopeTerm' {key} -> key) (\s@TagScopeTerm' {} a -> s {key = a} :: TagScopeTerm)

-- | The tag keys or tag key and value pairs to use in the condition. To
-- specify only tag keys in a condition, specify the keys in this array and
-- set the value for each associated tag value to an empty string.
tagScopeTerm_tagValues :: Lens.Lens' TagScopeTerm (Prelude.Maybe [TagValuePair])
tagScopeTerm_tagValues = Lens.lens (\TagScopeTerm' {tagValues} -> tagValues) (\s@TagScopeTerm' {} a -> s {tagValues = a} :: TagScopeTerm) Prelude.. Lens.mapping Lens.coerced

-- | The type of object to apply the condition to.
tagScopeTerm_target :: Lens.Lens' TagScopeTerm (Prelude.Maybe TagTarget)
tagScopeTerm_target = Lens.lens (\TagScopeTerm' {target} -> target) (\s@TagScopeTerm' {} a -> s {target = a} :: TagScopeTerm)

-- | The operator to use in the condition. Valid values are EQ (equals) or NE
-- (not equals).
tagScopeTerm_comparator :: Lens.Lens' TagScopeTerm (Prelude.Maybe JobComparator)
tagScopeTerm_comparator = Lens.lens (\TagScopeTerm' {comparator} -> comparator) (\s@TagScopeTerm' {} a -> s {comparator = a} :: TagScopeTerm)

instance Core.FromJSON TagScopeTerm where
  parseJSON =
    Core.withObject
      "TagScopeTerm"
      ( \x ->
          TagScopeTerm'
            Prelude.<$> (x Core..:? "key")
            Prelude.<*> (x Core..:? "tagValues" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "target")
            Prelude.<*> (x Core..:? "comparator")
      )

instance Prelude.Hashable TagScopeTerm where
  hashWithSalt _salt TagScopeTerm' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` tagValues
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` comparator

instance Prelude.NFData TagScopeTerm where
  rnf TagScopeTerm' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf tagValues
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf comparator

instance Core.ToJSON TagScopeTerm where
  toJSON TagScopeTerm' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("key" Core..=) Prelude.<$> key,
            ("tagValues" Core..=) Prelude.<$> tagValues,
            ("target" Core..=) Prelude.<$> target,
            ("comparator" Core..=) Prelude.<$> comparator
          ]
      )
