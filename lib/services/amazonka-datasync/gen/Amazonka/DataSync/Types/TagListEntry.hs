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
-- Module      : Amazonka.DataSync.Types.TagListEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.TagListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a single entry in a list of Amazon Web Services resource
-- tags. @TagListEntry@ returns an array that contains a list of tasks when
-- the
-- <https://docs.aws.amazon.com/datasync/latest/userguide/API_ListTagsForResource.html ListTagsForResource>
-- operation is called.
--
-- /See:/ 'newTagListEntry' smart constructor.
data TagListEntry = TagListEntry'
  { -- | The value for an Amazon Web Services resource tag.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key for an Amazon Web Services resource tag.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'tagListEntry_value' - The value for an Amazon Web Services resource tag.
--
-- 'key', 'tagListEntry_key' - The key for an Amazon Web Services resource tag.
newTagListEntry ::
  -- | 'key'
  Prelude.Text ->
  TagListEntry
newTagListEntry pKey_ =
  TagListEntry' {value = Prelude.Nothing, key = pKey_}

-- | The value for an Amazon Web Services resource tag.
tagListEntry_value :: Lens.Lens' TagListEntry (Prelude.Maybe Prelude.Text)
tagListEntry_value = Lens.lens (\TagListEntry' {value} -> value) (\s@TagListEntry' {} a -> s {value = a} :: TagListEntry)

-- | The key for an Amazon Web Services resource tag.
tagListEntry_key :: Lens.Lens' TagListEntry Prelude.Text
tagListEntry_key = Lens.lens (\TagListEntry' {key} -> key) (\s@TagListEntry' {} a -> s {key = a} :: TagListEntry)

instance Core.FromJSON TagListEntry where
  parseJSON =
    Core.withObject
      "TagListEntry"
      ( \x ->
          TagListEntry'
            Prelude.<$> (x Core..:? "Value") Prelude.<*> (x Core..: "Key")
      )

instance Prelude.Hashable TagListEntry where
  hashWithSalt _salt TagListEntry' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` key

instance Prelude.NFData TagListEntry where
  rnf TagListEntry' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

instance Core.ToJSON TagListEntry where
  toJSON TagListEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            Prelude.Just ("Key" Core..= key)
          ]
      )
