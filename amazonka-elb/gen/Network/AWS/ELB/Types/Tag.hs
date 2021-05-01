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
-- Module      : Network.AWS.ELB.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Tag where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a tag.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The value of the tag.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key of the tag.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'tag_value' - The value of the tag.
--
-- 'key', 'tag_key' - The key of the tag.
newTag ::
  -- | 'key'
  Prelude.Text ->
  Tag
newTag pKey_ =
  Tag' {value = Prelude.Nothing, key = pKey_}

-- | The value of the tag.
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

-- | The key of the tag.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

instance Prelude.FromXML Tag where
  parseXML x =
    Tag'
      Prelude.<$> (x Prelude..@? "Value")
      Prelude.<*> (x Prelude..@ "Key")

instance Prelude.Hashable Tag

instance Prelude.NFData Tag

instance Prelude.ToQuery Tag where
  toQuery Tag' {..} =
    Prelude.mconcat
      ["Value" Prelude.=: value, "Key" Prelude.=: key]
