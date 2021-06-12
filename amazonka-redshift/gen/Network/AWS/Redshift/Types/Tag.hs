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
-- Module      : Network.AWS.Redshift.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | A tag consisting of a name\/value pair for a resource.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The key, or name, for the resource tag.
    key :: Core.Maybe Core.Text,
    -- | The value for the resource tag.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Tag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tag_key' - The key, or name, for the resource tag.
--
-- 'value', 'tag_value' - The value for the resource tag.
newTag ::
  Tag
newTag =
  Tag' {key = Core.Nothing, value = Core.Nothing}

-- | The key, or name, for the resource tag.
tag_key :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value for the resource tag.
tag_value :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromXML Tag where
  parseXML x =
    Tag'
      Core.<$> (x Core..@? "Key") Core.<*> (x Core..@? "Value")

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToQuery Tag where
  toQuery Tag' {..} =
    Core.mconcat
      ["Key" Core.=: key, "Value" Core.=: value]
