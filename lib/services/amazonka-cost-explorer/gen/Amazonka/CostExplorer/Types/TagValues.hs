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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.TagValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.MatchOption
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The values that are available for a tag.
--
-- If @Values@ and @Key@ aren\'t specified, the @ABSENT@ @MatchOption@ is
-- applied to all tags. That is, it\'s filtered on resources with no tags.
--
-- If @Key@ is provided and @Values@ isn\'t specified, the @ABSENT@
-- @MatchOption@ is applied to the tag @Key@ only. That is, it\'s filtered
-- on resources without the given tag key.
--
-- /See:/ 'newTagValues' smart constructor.
data TagValues = TagValues'
  { -- | The key for the tag.
    key :: Prelude.Maybe Prelude.Text,
    -- | The match options that you can use to filter your results.
    -- @MatchOptions@ is only applicable for actions related to Cost Category.
    -- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
    matchOptions :: Prelude.Maybe [MatchOption],
    -- | The specific value of the tag.
    values :: Prelude.Maybe [Prelude.Text]
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
-- 'key', 'tagValues_key' - The key for the tag.
--
-- 'matchOptions', 'tagValues_matchOptions' - The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
--
-- 'values', 'tagValues_values' - The specific value of the tag.
newTagValues ::
  TagValues
newTagValues =
  TagValues'
    { key = Prelude.Nothing,
      matchOptions = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The key for the tag.
tagValues_key :: Lens.Lens' TagValues (Prelude.Maybe Prelude.Text)
tagValues_key = Lens.lens (\TagValues' {key} -> key) (\s@TagValues' {} a -> s {key = a} :: TagValues)

-- | The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
tagValues_matchOptions :: Lens.Lens' TagValues (Prelude.Maybe [MatchOption])
tagValues_matchOptions = Lens.lens (\TagValues' {matchOptions} -> matchOptions) (\s@TagValues' {} a -> s {matchOptions = a} :: TagValues) Prelude.. Lens.mapping Lens.coerced

-- | The specific value of the tag.
tagValues_values :: Lens.Lens' TagValues (Prelude.Maybe [Prelude.Text])
tagValues_values = Lens.lens (\TagValues' {values} -> values) (\s@TagValues' {} a -> s {values = a} :: TagValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TagValues where
  parseJSON =
    Data.withObject
      "TagValues"
      ( \x ->
          TagValues'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "MatchOptions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TagValues where
  hashWithSalt _salt TagValues' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` matchOptions
      `Prelude.hashWithSalt` values

instance Prelude.NFData TagValues where
  rnf TagValues' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf matchOptions
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON TagValues where
  toJSON TagValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("MatchOptions" Data..=) Prelude.<$> matchOptions,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
