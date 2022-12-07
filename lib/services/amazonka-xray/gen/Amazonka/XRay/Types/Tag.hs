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
-- Module      : Amazonka.XRay.Types.Tag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A map that contains tag keys and tag values to attach to an Amazon Web
-- Services X-Ray group or sampling rule. For more information about ways
-- to use tags, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference/.
--
-- The following restrictions apply to tags:
--
-- -   Maximum number of user-applied tags per resource: 50
--
-- -   Tag keys and values are case sensitive.
--
-- -   Don\'t use @aws:@ as a prefix for keys; it\'s reserved for Amazon
--     Web Services use. You cannot edit or delete system tags.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | A tag key, such as @Stage@ or @Name@. A tag key cannot be empty. The key
    -- can be a maximum of 128 characters, and can contain only Unicode
    -- letters, numbers, or separators, or the following special characters:
    -- @+ - = . _ : \/@
    key :: Prelude.Text,
    -- | An optional tag value, such as @Production@ or @test-only@. The value
    -- can be a maximum of 255 characters, and contain only Unicode letters,
    -- numbers, or separators, or the following special characters:
    -- @+ - = . _ : \/@
    value :: Prelude.Text
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
-- 'key', 'tag_key' - A tag key, such as @Stage@ or @Name@. A tag key cannot be empty. The key
-- can be a maximum of 128 characters, and can contain only Unicode
-- letters, numbers, or separators, or the following special characters:
-- @+ - = . _ : \/@
--
-- 'value', 'tag_value' - An optional tag value, such as @Production@ or @test-only@. The value
-- can be a maximum of 255 characters, and contain only Unicode letters,
-- numbers, or separators, or the following special characters:
-- @+ - = . _ : \/@
newTag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Tag
newTag pKey_ pValue_ =
  Tag' {key = pKey_, value = pValue_}

-- | A tag key, such as @Stage@ or @Name@. A tag key cannot be empty. The key
-- can be a maximum of 128 characters, and can contain only Unicode
-- letters, numbers, or separators, or the following special characters:
-- @+ - = . _ : \/@
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | An optional tag value, such as @Production@ or @test-only@. The value
-- can be a maximum of 255 characters, and contain only Unicode letters,
-- numbers, or separators, or the following special characters:
-- @+ - = . _ : \/@
tag_value :: Lens.Lens' Tag Prelude.Text
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Data.FromJSON Tag where
  parseJSON =
    Data.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Data..: "Key") Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Tag where
  toJSON Tag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Value" Data..= value)
          ]
      )
