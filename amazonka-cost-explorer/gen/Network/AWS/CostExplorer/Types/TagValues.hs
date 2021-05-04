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
-- Module      : Network.AWS.CostExplorer.Types.TagValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.TagValues where

import Network.AWS.CostExplorer.Types.MatchOption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The values that are available for a tag.
--
-- If @Values@ and @Key@ are not specified, the @ABSENT@ @MatchOption@ is
-- applied to all tags. That is, filtering on resources with no tags.
--
-- If @Values@ is provided and @Key@ is not specified, the @ABSENT@
-- @MatchOption@ is applied to the tag @Key@ only. That is, filtering on
-- resources without the given tag key.
--
-- /See:/ 'newTagValues' smart constructor.
data TagValues = TagValues'
  { -- | The key for the tag.
    key :: Prelude.Maybe Prelude.Text,
    -- | The specific value of the tag.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The match options that you can use to filter your results.
    -- @MatchOptions@ is only applicable for actions related to Cost Category.
    -- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
    matchOptions :: Prelude.Maybe [MatchOption]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'values', 'tagValues_values' - The specific value of the tag.
--
-- 'matchOptions', 'tagValues_matchOptions' - The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
newTagValues ::
  TagValues
newTagValues =
  TagValues'
    { key = Prelude.Nothing,
      values = Prelude.Nothing,
      matchOptions = Prelude.Nothing
    }

-- | The key for the tag.
tagValues_key :: Lens.Lens' TagValues (Prelude.Maybe Prelude.Text)
tagValues_key = Lens.lens (\TagValues' {key} -> key) (\s@TagValues' {} a -> s {key = a} :: TagValues)

-- | The specific value of the tag.
tagValues_values :: Lens.Lens' TagValues (Prelude.Maybe [Prelude.Text])
tagValues_values = Lens.lens (\TagValues' {values} -> values) (\s@TagValues' {} a -> s {values = a} :: TagValues) Prelude.. Lens.mapping Prelude._Coerce

-- | The match options that you can use to filter your results.
-- @MatchOptions@ is only applicable for actions related to Cost Category.
-- The default values for @MatchOptions@ are @EQUALS@ and @CASE_SENSITIVE@.
tagValues_matchOptions :: Lens.Lens' TagValues (Prelude.Maybe [MatchOption])
tagValues_matchOptions = Lens.lens (\TagValues' {matchOptions} -> matchOptions) (\s@TagValues' {} a -> s {matchOptions = a} :: TagValues) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON TagValues where
  parseJSON =
    Prelude.withObject
      "TagValues"
      ( \x ->
          TagValues'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Values" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "MatchOptions"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TagValues

instance Prelude.NFData TagValues

instance Prelude.ToJSON TagValues where
  toJSON TagValues' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Values" Prelude..=) Prelude.<$> values,
            ("MatchOptions" Prelude..=)
              Prelude.<$> matchOptions
          ]
      )
