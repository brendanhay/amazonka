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
-- Module      : Network.AWS.RDS.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Tag where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Metadata assigned to an Amazon RDS resource consisting of a key-value
-- pair.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | A key is the required name of the tag. The string value can be from 1 to
    -- 128 Unicode characters in length and can\'t be prefixed with \"aws:\" or
    -- \"rds:\". The string can only contain only the set of Unicode letters,
    -- digits, white-space, \'_\', \'.\', \':\', \'\/\', \'=\', \'+\', \'-\',
    -- \'\@\' (Java regex: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-\@]*)$\").
    key :: Core.Maybe Core.Text,
    -- | A value is the optional value of the tag. The string value can be from 1
    -- to 256 Unicode characters in length and can\'t be prefixed with \"aws:\"
    -- or \"rds:\". The string can only contain only the set of Unicode
    -- letters, digits, white-space, \'_\', \'.\', \':\', \'\/\', \'=\', \'+\',
    -- \'-\', \'\@\' (Java regex:
    -- \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-\@]*)$\").
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
-- 'key', 'tag_key' - A key is the required name of the tag. The string value can be from 1 to
-- 128 Unicode characters in length and can\'t be prefixed with \"aws:\" or
-- \"rds:\". The string can only contain only the set of Unicode letters,
-- digits, white-space, \'_\', \'.\', \':\', \'\/\', \'=\', \'+\', \'-\',
-- \'\@\' (Java regex: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-\@]*)$\").
--
-- 'value', 'tag_value' - A value is the optional value of the tag. The string value can be from 1
-- to 256 Unicode characters in length and can\'t be prefixed with \"aws:\"
-- or \"rds:\". The string can only contain only the set of Unicode
-- letters, digits, white-space, \'_\', \'.\', \':\', \'\/\', \'=\', \'+\',
-- \'-\', \'\@\' (Java regex:
-- \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-\@]*)$\").
newTag ::
  Tag
newTag =
  Tag' {key = Core.Nothing, value = Core.Nothing}

-- | A key is the required name of the tag. The string value can be from 1 to
-- 128 Unicode characters in length and can\'t be prefixed with \"aws:\" or
-- \"rds:\". The string can only contain only the set of Unicode letters,
-- digits, white-space, \'_\', \'.\', \':\', \'\/\', \'=\', \'+\', \'-\',
-- \'\@\' (Java regex: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-\@]*)$\").
tag_key :: Lens.Lens' Tag (Core.Maybe Core.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | A value is the optional value of the tag. The string value can be from 1
-- to 256 Unicode characters in length and can\'t be prefixed with \"aws:\"
-- or \"rds:\". The string can only contain only the set of Unicode
-- letters, digits, white-space, \'_\', \'.\', \':\', \'\/\', \'=\', \'+\',
-- \'-\', \'\@\' (Java regex:
-- \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-\@]*)$\").
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
