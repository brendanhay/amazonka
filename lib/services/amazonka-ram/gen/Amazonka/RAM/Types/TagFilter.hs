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
-- Module      : Amazonka.RAM.Types.TagFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.TagFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A tag key and optional list of possible values that you can use to
-- filter results for tagged resources.
--
-- /See:/ 'newTagFilter' smart constructor.
data TagFilter = TagFilter'
  { -- | The tag key. This must have a valid string value and can\'t be empty.
    tagKey :: Prelude.Maybe Prelude.Text,
    -- | A list of zero or more tag values. If no values are provided, then the
    -- filter matches any tag with the specified key, regardless of its value.
    tagValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagKey', 'tagFilter_tagKey' - The tag key. This must have a valid string value and can\'t be empty.
--
-- 'tagValues', 'tagFilter_tagValues' - A list of zero or more tag values. If no values are provided, then the
-- filter matches any tag with the specified key, regardless of its value.
newTagFilter ::
  TagFilter
newTagFilter =
  TagFilter'
    { tagKey = Prelude.Nothing,
      tagValues = Prelude.Nothing
    }

-- | The tag key. This must have a valid string value and can\'t be empty.
tagFilter_tagKey :: Lens.Lens' TagFilter (Prelude.Maybe Prelude.Text)
tagFilter_tagKey = Lens.lens (\TagFilter' {tagKey} -> tagKey) (\s@TagFilter' {} a -> s {tagKey = a} :: TagFilter)

-- | A list of zero or more tag values. If no values are provided, then the
-- filter matches any tag with the specified key, regardless of its value.
tagFilter_tagValues :: Lens.Lens' TagFilter (Prelude.Maybe [Prelude.Text])
tagFilter_tagValues = Lens.lens (\TagFilter' {tagValues} -> tagValues) (\s@TagFilter' {} a -> s {tagValues = a} :: TagFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable TagFilter where
  hashWithSalt _salt TagFilter' {..} =
    _salt `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData TagFilter where
  rnf TagFilter' {..} =
    Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagValues

instance Data.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tagKey" Data..=) Prelude.<$> tagKey,
            ("tagValues" Data..=) Prelude.<$> tagValues
          ]
      )
