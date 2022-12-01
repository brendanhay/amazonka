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
-- Module      : Amazonka.DMS.Types.Tag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A user-defined key-value pair that describes metadata added to an DMS
-- resource and that is used by operations such as the following:
--
-- -   @AddTagsToResource@
--
-- -   @ListTagsForResource@
--
-- -   @RemoveTagsFromResource@
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | A key is the required name of the tag. The string value can be 1-128
    -- Unicode characters in length and can\'t be prefixed with \"aws:\" or
    -- \"dms:\". The string can only contain only the set of Unicode letters,
    -- digits, white-space, \'_\', \'.\', \'\/\', \'=\', \'+\', \'-\' (Java
    -- regular expressions: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-]*)$\").
    key :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- resource for which the tag is created.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | A value is the optional value of the tag. The string value can be 1-256
    -- Unicode characters in length and can\'t be prefixed with \"aws:\" or
    -- \"dms:\". The string can only contain only the set of Unicode letters,
    -- digits, white-space, \'_\', \'.\', \'\/\', \'=\', \'+\', \'-\' (Java
    -- regular expressions: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-]*)$\").
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
-- 'key', 'tag_key' - A key is the required name of the tag. The string value can be 1-128
-- Unicode characters in length and can\'t be prefixed with \"aws:\" or
-- \"dms:\". The string can only contain only the set of Unicode letters,
-- digits, white-space, \'_\', \'.\', \'\/\', \'=\', \'+\', \'-\' (Java
-- regular expressions: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-]*)$\").
--
-- 'resourceArn', 'tag_resourceArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- resource for which the tag is created.
--
-- 'value', 'tag_value' - A value is the optional value of the tag. The string value can be 1-256
-- Unicode characters in length and can\'t be prefixed with \"aws:\" or
-- \"dms:\". The string can only contain only the set of Unicode letters,
-- digits, white-space, \'_\', \'.\', \'\/\', \'=\', \'+\', \'-\' (Java
-- regular expressions: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-]*)$\").
newTag ::
  Tag
newTag =
  Tag'
    { key = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A key is the required name of the tag. The string value can be 1-128
-- Unicode characters in length and can\'t be prefixed with \"aws:\" or
-- \"dms:\". The string can only contain only the set of Unicode letters,
-- digits, white-space, \'_\', \'.\', \'\/\', \'=\', \'+\', \'-\' (Java
-- regular expressions: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-]*)$\").
tag_key :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- resource for which the tag is created.
tag_resourceArn :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_resourceArn = Lens.lens (\Tag' {resourceArn} -> resourceArn) (\s@Tag' {} a -> s {resourceArn = a} :: Tag)

-- | A value is the optional value of the tag. The string value can be 1-256
-- Unicode characters in length and can\'t be prefixed with \"aws:\" or
-- \"dms:\". The string can only contain only the set of Unicode letters,
-- digits, white-space, \'_\', \'.\', \'\/\', \'=\', \'+\', \'-\' (Java
-- regular expressions: \"^([\\\\p{L}\\\\p{Z}\\\\p{N}_.:\/=+\\\\-]*)$\").
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "ResourceArn")
            Prelude.<*> (x Core..:? "Value")
      )

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` value

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Key" Core..=) Prelude.<$> key,
            ("ResourceArn" Core..=) Prelude.<$> resourceArn,
            ("Value" Core..=) Prelude.<$> value
          ]
      )
