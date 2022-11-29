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
-- Module      : Amazonka.FMS.Types.Tag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.Tag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A collection of key:value pairs associated with an Amazon Web Services
-- resource. The key:value pair can be anything you define. Typically, the
-- tag key represents a category (such as \"environment\") and the tag
-- value represents a specific value within that category (such as
-- \"test,\" \"development,\" or \"production\"). You can add up to 50 tags
-- to each Amazon Web Services resource.
--
-- /See:/ 'newTag' smart constructor.
data Tag = Tag'
  { -- | Part of the key:value pair that defines a tag. You can use a tag key to
    -- describe a category of information, such as \"customer.\" Tag keys are
    -- case-sensitive.
    key :: Prelude.Text,
    -- | Part of the key:value pair that defines a tag. You can use a tag value
    -- to describe a specific value within a category, such as \"companyA\" or
    -- \"companyB.\" Tag values are case-sensitive.
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
-- 'key', 'tag_key' - Part of the key:value pair that defines a tag. You can use a tag key to
-- describe a category of information, such as \"customer.\" Tag keys are
-- case-sensitive.
--
-- 'value', 'tag_value' - Part of the key:value pair that defines a tag. You can use a tag value
-- to describe a specific value within a category, such as \"companyA\" or
-- \"companyB.\" Tag values are case-sensitive.
newTag ::
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Tag
newTag pKey_ pValue_ =
  Tag' {key = pKey_, value = pValue_}

-- | Part of the key:value pair that defines a tag. You can use a tag key to
-- describe a category of information, such as \"customer.\" Tag keys are
-- case-sensitive.
tag_key :: Lens.Lens' Tag Prelude.Text
tag_key = Lens.lens (\Tag' {key} -> key) (\s@Tag' {} a -> s {key = a} :: Tag)

-- | Part of the key:value pair that defines a tag. You can use a tag value
-- to describe a specific value within a category, such as \"companyA\" or
-- \"companyB.\" Tag values are case-sensitive.
tag_value :: Lens.Lens' Tag Prelude.Text
tag_value = Lens.lens (\Tag' {value} -> value) (\s@Tag' {} a -> s {value = a} :: Tag)

instance Core.FromJSON Tag where
  parseJSON =
    Core.withObject
      "Tag"
      ( \x ->
          Tag'
            Prelude.<$> (x Core..: "Key") Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable Tag where
  hashWithSalt _salt Tag' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Tag where
  rnf Tag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Core.ToJSON Tag where
  toJSON Tag' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Value" Core..= value)
          ]
      )
