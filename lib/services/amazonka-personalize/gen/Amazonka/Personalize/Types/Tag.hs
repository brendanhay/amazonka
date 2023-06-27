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
-- Module      : Amazonka.Personalize.Types.Tag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The optional metadata that you apply to resources to help you categorize
-- and organize them. Each tag consists of a key and an optional value,
-- both of which you define. For more information see
-- <https://docs.aws.amazon.com/personalize/latest/dg/tagging-resources.html Tagging Amazon Personalize recources>.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | One part of a key-value pair that makes up a tag. A key is a general
    -- label that acts like a category for more specific tag values.
    tagKey :: Prelude.Text,
    -- | The optional part of a key-value pair that makes up a tag. A value acts
    -- as a descriptor within a tag category (key).
    tagValue :: Prelude.Text
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
-- 'tagKey', 'tag_tagKey' - One part of a key-value pair that makes up a tag. A key is a general
-- label that acts like a category for more specific tag values.
--
-- 'tagValue', 'tag_tagValue' - The optional part of a key-value pair that makes up a tag. A value acts
-- as a descriptor within a tag category (key).
newTag ::
  -- | 'tagKey'
  Prelude.Text ->
  -- | 'tagValue'
  Prelude.Text ->
  Tag
newTag pTagKey_ pTagValue_ =
  Tag' {tagKey = pTagKey_, tagValue = pTagValue_}

-- | One part of a key-value pair that makes up a tag. A key is a general
-- label that acts like a category for more specific tag values.
tag_tagKey :: Lens.Lens' Tag Prelude.Text
tag_tagKey = Lens.lens (\Tag' {tagKey} -> tagKey) (\s@Tag' {} a -> s {tagKey = a} :: Tag)

-- | The optional part of a key-value pair that makes up a tag. A value acts
-- as a descriptor within a tag category (key).
tag_tagValue :: Lens.Lens' Tag Prelude.Text
tag_tagValue = Lens.lens (\Tag' {tagValue} -> tagValue) (\s@Tag' {} a -> s {tagValue = a} :: Tag)

instance Data.FromJSON Tag where
  parseJSON =
    Data.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Data..: "tagKey")
            Prelude.<*> (x Data..: "tagValue")
      )

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt
      `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` tagValue

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagValue

instance Data.ToJSON Tag where
  toJSON Tag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("tagKey" Data..= tagKey),
            Prelude.Just ("tagValue" Data..= tagValue)
          ]
      )
