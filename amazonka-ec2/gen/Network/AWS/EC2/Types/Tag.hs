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
-- Module      : Network.AWS.EC2.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Tag where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a tag.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | The key of the tag.
    --
    -- Constraints: Tag keys are case-sensitive and accept a maximum of 127
    -- Unicode characters. May not begin with @aws:@.
    key :: Core.Text,
    -- | The value of the tag.
    --
    -- Constraints: Tag values are case-sensitive and accept a maximum of 255
    -- Unicode characters.
    value :: Core.Text
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
-- 'key', 'tag_key' - The key of the tag.
--
-- Constraints: Tag keys are case-sensitive and accept a maximum of 127
-- Unicode characters. May not begin with @aws:@.
--
-- 'value', 'tag_value' - The value of the tag.
--
-- Constraints: Tag values are case-sensitive and accept a maximum of 255
-- Unicode characters.
newTag ::
  -- | 'key'
  Core.Text ->
  -- | 'value'
  Core.Text ->
  Tag
newTag pKey_ pValue_ =
  Tag' {key = pKey_, value = pValue_}

-- | The key of the tag.
--
-- Constraints: Tag keys are case-sensitive and accept a maximum of 127
-- Unicode characters. May not begin with @aws:@.
tag_key :: Lens.Lens' Tag Core.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The value of the tag.
--
-- Constraints: Tag values are case-sensitive and accept a maximum of 255
-- Unicode characters.
tag_value :: Lens.Lens' Tag Core.Text
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromXML Tag where
  parseXML x =
    Tag'
      Core.<$> (x Core..@ "key") Core.<*> (x Core..@ "value")

instance Core.Hashable Tag

instance Core.NFData Tag

instance Core.ToQuery Tag where
  toQuery Tag' {..} =
    Core.mconcat
      ["Key" Core.=: key, "Value" Core.=: value]
