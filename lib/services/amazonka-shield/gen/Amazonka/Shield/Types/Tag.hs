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
-- Module      : Amazonka.Shield.Types.Tag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A tag associated with an Amazon Web Services resource. Tags are
-- key:value pairs that you can use to categorize and manage your
-- resources, for purposes like billing or other management. Typically, the
-- tag key represents a category, such as \"environment\", and the tag
-- value represents a specific value within that category, such as
-- \"test,\" \"development,\" or \"production\". Or you might set the tag
-- key to \"customer\" and the value to the customer name or ID. You can
-- specify one or more tags to add to each Amazon Web Services resource, up
-- to 50 tags for a resource.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | Part of the key:value pair that defines a tag. You can use a tag key to
    -- describe a category of information, such as \"customer.\" Tag keys are
    -- case-sensitive.
    key :: Prelude.Maybe Prelude.Text,
    -- | Part of the key:value pair that defines a tag. You can use a tag value
    -- to describe a specific value within a category, such as \"companyA\" or
    -- \"companyB.\" Tag values are case-sensitive.
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
-- 'key', 'tag_key' - Part of the key:value pair that defines a tag. You can use a tag key to
-- describe a category of information, such as \"customer.\" Tag keys are
-- case-sensitive.
--
-- 'value', 'tag_value' - Part of the key:value pair that defines a tag. You can use a tag value
-- to describe a specific value within a category, such as \"companyA\" or
-- \"companyB.\" Tag values are case-sensitive.
newTag ::
  Tag
newTag =
  Tag'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | Part of the key:value pair that defines a tag. You can use a tag key to
-- describe a category of information, such as \"customer.\" Tag keys are
-- case-sensitive.
tag_key :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | Part of the key:value pair that defines a tag. You can use a tag value
-- to describe a specific value within a category, such as \"companyA\" or
-- \"companyB.\" Tag values are case-sensitive.
tag_value :: Lens.Lens' Tag (Prelude.Maybe Prelude.Text)
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Data.FromJSON Tag where
  parseJSON =
    Data.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Tag where
  toJSON Tag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
