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
-- Module      : Network.AWS.Athena.Types.Tag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.Tag where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A label that you assign to a resource. In Athena, a resource can be a
-- workgroup or data catalog. Each tag consists of a key and an optional
-- value, both of which you define. For example, you can use tags to
-- categorize Athena workgroups or data catalogs by purpose, owner, or
-- environment. Use a consistent set of tag keys to make it easier to
-- search and filter workgroups or data catalogs in your account. For best
-- practices, see
-- <https://aws.amazon.com/answers/account-management/aws-tagging-strategies/ Tagging Best Practices>.
-- Tag keys can be from 1 to 128 UTF-8 Unicode characters, and tag values
-- can be from 0 to 256 UTF-8 Unicode characters. Tags can use letters and
-- numbers representable in UTF-8, and the following characters: + - = . _
-- : \/ \@. Tag keys and values are case-sensitive. Tag keys must be unique
-- per resource. If you specify more than one tag, separate them by commas.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | A tag key. The tag key length is from 1 to 128 Unicode characters in
    -- UTF-8. You can use letters and numbers representable in UTF-8, and the
    -- following characters: + - = . _ : \/ \@. Tag keys are case-sensitive and
    -- must be unique per resource.
    key :: Prelude.Maybe Prelude.Text,
    -- | A tag value. The tag value length is from 0 to 256 Unicode characters in
    -- UTF-8. You can use letters and numbers representable in UTF-8, and the
    -- following characters: + - = . _ : \/ \@. Tag values are case-sensitive.
    value :: Prelude.Maybe Prelude.Text
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
-- 'key', 'tag_key' - A tag key. The tag key length is from 1 to 128 Unicode characters in
-- UTF-8. You can use letters and numbers representable in UTF-8, and the
-- following characters: + - = . _ : \/ \@. Tag keys are case-sensitive and
-- must be unique per resource.
--
-- 'value', 'tag_value' - A tag value. The tag value length is from 0 to 256 Unicode characters in
-- UTF-8. You can use letters and numbers representable in UTF-8, and the
-- following characters: + - = . _ : \/ \@. Tag values are case-sensitive.
newTag ::
  Tag
newTag =
  Tag'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A tag key. The tag key length is from 1 to 128 Unicode characters in
-- UTF-8. You can use letters and numbers representable in UTF-8, and the
-- following characters: + - = . _ : \/ \@. Tag keys are case-sensitive and
-- must be unique per resource.
tag_key :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | A tag value. The tag value length is from 0 to 256 Unicode characters in
-- UTF-8. You can use letters and numbers representable in UTF-8, and the
-- following characters: + - = . _ : \/ \@. Tag values are case-sensitive.
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Prelude.FromJSON Tag where
  parseJSON =
    Prelude.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable Tag

instance Prelude.NFData Tag

instance Prelude.ToJSON Tag where
  toJSON Tag' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Value" Prelude..=) Prelude.<$> value
          ]
      )
