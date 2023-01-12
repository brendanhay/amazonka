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
-- Module      : Amazonka.SESV2.Types.Tag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that defines the tags that are associated with a resource.
-- A /tag/ is a label that you optionally define and associate with a
-- resource. Tags can help you categorize and manage resources in different
-- ways, such as by purpose, owner, environment, or other criteria. A
-- resource can have as many as 50 tags.
--
-- Each tag consists of a required /tag key/ and an associated /tag value/,
-- both of which you define. A tag key is a general label that acts as a
-- category for a more specific tag value. A tag value acts as a descriptor
-- within a tag key. A tag key can contain as many as 128 characters. A tag
-- value can contain as many as 256 characters. The characters can be
-- Unicode letters, digits, white space, or one of the following symbols: _
-- . : \/ = + -. The following additional restrictions apply to tags:
--
-- -   Tag keys and values are case sensitive.
--
-- -   For each associated resource, each tag key must be unique and it can
--     have only one value.
--
-- -   The @aws:@ prefix is reserved for use by Amazon Web Services; you
--     can’t use it in any tag keys or values that you define. In addition,
--     you can\'t edit or remove tag keys or values that use this prefix.
--     Tags that use this prefix don’t count against the limit of 50 tags
--     per resource.
--
-- -   You can associate tags with public or shared resources, but the tags
--     are available only for your Amazon Web Services account, not any
--     other accounts that share the resource. In addition, the tags are
--     available only for resources that are located in the specified
--     Amazon Web Services Region for your Amazon Web Services account.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | One part of a key-value pair that defines a tag. The maximum length of a
    -- tag key is 128 characters. The minimum length is 1 character.
    key :: Prelude.Text,
    -- | The optional part of a key-value pair that defines a tag. The maximum
    -- length of a tag value is 256 characters. The minimum length is 0
    -- characters. If you don\'t want a resource to have a specific tag value,
    -- don\'t specify a value for this parameter. If you don\'t specify a
    -- value, Amazon SES sets the value to an empty string.
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
-- 'key', 'tag_key' - One part of a key-value pair that defines a tag. The maximum length of a
-- tag key is 128 characters. The minimum length is 1 character.
--
-- 'value', 'tag_value' - The optional part of a key-value pair that defines a tag. The maximum
-- length of a tag value is 256 characters. The minimum length is 0
-- characters. If you don\'t want a resource to have a specific tag value,
-- don\'t specify a value for this parameter. If you don\'t specify a
-- value, Amazon SES sets the value to an empty string.
newTag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Tag
newTag pKey_ pValue_ =
  Tag' {key = pKey_, value = pValue_}

-- | One part of a key-value pair that defines a tag. The maximum length of a
-- tag key is 128 characters. The minimum length is 1 character.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | The optional part of a key-value pair that defines a tag. The maximum
-- length of a tag value is 256 characters. The minimum length is 0
-- characters. If you don\'t want a resource to have a specific tag value,
-- don\'t specify a value for this parameter. If you don\'t specify a
-- value, Amazon SES sets the value to an empty string.
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
