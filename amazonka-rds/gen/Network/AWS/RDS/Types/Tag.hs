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
import qualified Network.AWS.Prelude as Prelude

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
    key :: Prelude.Maybe Prelude.Text,
    -- | A value is the optional value of the tag. The string value can be from 1
    -- to 256 Unicode characters in length and can\'t be prefixed with \"aws:\"
    -- or \"rds:\". The string can only contain only the set of Unicode
    -- letters, digits, white-space, \'_\', \'.\', \':\', \'\/\', \'=\', \'+\',
    -- \'-\', \'\@\' (Java regex:
    -- \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-\@]*)$\").
    value :: Prelude.Maybe Prelude.Text
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
  Tag'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A key is the required name of the tag. The string value can be from 1 to
-- 128 Unicode characters in length and can\'t be prefixed with \"aws:\" or
-- \"rds:\". The string can only contain only the set of Unicode letters,
-- digits, white-space, \'_\', \'.\', \':\', \'\/\', \'=\', \'+\', \'-\',
-- \'\@\' (Java regex: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-\@]*)$\").
tag_key :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | A value is the optional value of the tag. The string value can be from 1
-- to 256 Unicode characters in length and can\'t be prefixed with \"aws:\"
-- or \"rds:\". The string can only contain only the set of Unicode
-- letters, digits, white-space, \'_\', \'.\', \':\', \'\/\', \'=\', \'+\',
-- \'-\', \'\@\' (Java regex:
-- \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-\@]*)$\").
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromXML Tag where
  parseXML x =
    Tag'
      Prelude.<$> (x Core..@? "Key") Prelude.<*> (x Core..@? "Value")

instance Prelude.Hashable Tag

instance Prelude.NFData Tag

instance Core.ToQuery Tag where
  toQuery Tag' {..} =
    Prelude.mconcat
      ["Key" Core.=: key, "Value" Core.=: value]
