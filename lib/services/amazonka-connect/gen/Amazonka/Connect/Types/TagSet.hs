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
-- Module      : Amazonka.Connect.Types.TagSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.TagSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A tag set contains tag key and tag value.
--
-- /See:/ 'newTagSet' smart constructor.
data TagSet = TagSet'
  { -- | The tag key in the tagSet.
    key :: Prelude.Maybe Prelude.Text,
    -- | The tag value in the tagSet.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tagSet_key' - The tag key in the tagSet.
--
-- 'value', 'tagSet_value' - The tag value in the tagSet.
newTagSet ::
  TagSet
newTagSet =
  TagSet'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The tag key in the tagSet.
tagSet_key :: Lens.Lens' TagSet (Prelude.Maybe Prelude.Text)
tagSet_key = Lens.lens (\TagSet' {key} -> key) (\s@TagSet' {} a -> s {key = a} :: TagSet)

-- | The tag value in the tagSet.
tagSet_value :: Lens.Lens' TagSet (Prelude.Maybe Prelude.Text)
tagSet_value = Lens.lens (\TagSet' {value} -> value) (\s@TagSet' {} a -> s {value = a} :: TagSet)

instance Data.FromJSON TagSet where
  parseJSON =
    Data.withObject
      "TagSet"
      ( \x ->
          TagSet'
            Prelude.<$> (x Data..:? "key")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable TagSet where
  hashWithSalt _salt TagSet' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData TagSet where
  rnf TagSet' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value
