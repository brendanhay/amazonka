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
-- Module      : Amazonka.CostExplorer.Types.TagValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.TagValues where

import qualified Amazonka.Core as Core
import Amazonka.CostExplorer.Types.MatchOption
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The values that are available for a tag.
--
-- If @Values@ and @Key@ aren\'t specified, the @ABSENT@ @MatchOption@ is
-- applied to all tags. That is, it\'s filtered on resources with no tags.
--
-- If @Values@ is provided and @Key@ isn\'t specified, the @ABSENT@
-- @MatchOption@ is applied to the tag @Key@ only. That is, it\'s filtered
-- on resources without the given tag key.
--
-- /See:/ 'newTagValues' smart constructor.
data TagValues = TagValues'
  { -- | The specific value of the tag.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The key for the tag.
    key :: Prelude.Maybe Prelude.Text,
    -- | The match options that you can use to filter your results.
    -- @MatchOptions@ is only applicable for actions related to Cost Category.
    -- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
    matchOptions :: Prelude.Maybe [MatchOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'tagValues_values' - The specific value of the tag.
--
-- 'key', 'tagValues_key' - The key for the tag.
--
-- 'matchOptions', 'tagValues_matchOptions' - The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
newTagValues ::
  TagValues
newTagValues =
  TagValues'
    { values = Prelude.Nothing,
      key = Prelude.Nothing,
      matchOptions = Prelude.Nothing
    }

-- | The specific value of the tag.
tagValues_values :: Lens.Lens' TagValues (Prelude.Maybe [Prelude.Text])
tagValues_values = Lens.lens (\TagValues' {values} -> values) (\s@TagValues' {} a -> s {values = a} :: TagValues) Prelude.. Lens.mapping Lens.coerced

-- | The key for the tag.
tagValues_key :: Lens.Lens' TagValues (Prelude.Maybe Prelude.Text)
tagValues_key = Lens.lens (\TagValues' {key} -> key) (\s@TagValues' {} a -> s {key = a} :: TagValues)

-- | The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
tagValues_matchOptions :: Lens.Lens' TagValues (Prelude.Maybe [MatchOption])
tagValues_matchOptions = Lens.lens (\TagValues' {matchOptions} -> matchOptions) (\s@TagValues' {} a -> s {matchOptions = a} :: TagValues) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TagValues where
  parseJSON =
    Core.withObject
      "TagValues"
      ( \x ->
          TagValues'
            Prelude.<$> (x Core..:? "Values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "MatchOptions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TagValues

instance Prelude.NFData TagValues

instance Core.ToJSON TagValues where
  toJSON TagValues' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Values" Core..=) Prelude.<$> values,
            ("Key" Core..=) Prelude.<$> key,
            ("MatchOptions" Core..=) Prelude.<$> matchOptions
          ]
      )
