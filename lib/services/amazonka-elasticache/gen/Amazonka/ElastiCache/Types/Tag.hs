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
-- Module      : Amazonka.ElastiCache.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A tag that can be added to an ElastiCache cluster or replication group.
-- Tags are composed of a Key\/Value pair. You can use tags to categorize
-- and track all your ElastiCache resources, with the exception of global
-- replication group. When you add or remove tags on replication groups,
-- those actions will be replicated to all nodes in the replication group.
-- A tag with a null Value is permitted.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The tag\'s value. May be null.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key for the tag. May not be null.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'tag_value' - The tag\'s value. May be null.
--
-- 'key', 'tag_key' - The key for the tag. May not be null.
newTag ::
  Tag
newTag =
  Tag'
    { value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The tag\'s value. May be null.
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | The key for the tag. May not be null.
tag_key :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

instance Core.FromXML Tag where
  parseXML x =
    Tag'
      Prelude.<$> (x Core..@? "Value") Prelude.<*> (x Core..@? "Key")

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` key

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

instance Core.ToQuery Tag where
  toQuery Tag' {..} =
    Prelude.mconcat
      ["Value" Core.=: value, "Key" Core.=: key]
