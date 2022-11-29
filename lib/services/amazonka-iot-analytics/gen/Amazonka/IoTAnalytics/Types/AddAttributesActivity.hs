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
-- Module      : Amazonka.IoTAnalytics.Types.AddAttributesActivity
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.AddAttributesActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An activity that adds other attributes based on existing attributes in
-- the message.
--
-- /See:/ 'newAddAttributesActivity' smart constructor.
data AddAttributesActivity = AddAttributesActivity'
  { -- | The next activity in the pipeline.
    next :: Prelude.Maybe Prelude.Text,
    -- | The name of the addAttributes activity.
    name :: Prelude.Text,
    -- | A list of 1-50 @AttributeNameMapping@ objects that map an existing
    -- attribute to a new attribute.
    --
    -- The existing attributes remain in the message, so if you want to remove
    -- the originals, use @RemoveAttributeActivity@.
    attributes :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddAttributesActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'next', 'addAttributesActivity_next' - The next activity in the pipeline.
--
-- 'name', 'addAttributesActivity_name' - The name of the addAttributes activity.
--
-- 'attributes', 'addAttributesActivity_attributes' - A list of 1-50 @AttributeNameMapping@ objects that map an existing
-- attribute to a new attribute.
--
-- The existing attributes remain in the message, so if you want to remove
-- the originals, use @RemoveAttributeActivity@.
newAddAttributesActivity ::
  -- | 'name'
  Prelude.Text ->
  AddAttributesActivity
newAddAttributesActivity pName_ =
  AddAttributesActivity'
    { next = Prelude.Nothing,
      name = pName_,
      attributes = Prelude.mempty
    }

-- | The next activity in the pipeline.
addAttributesActivity_next :: Lens.Lens' AddAttributesActivity (Prelude.Maybe Prelude.Text)
addAttributesActivity_next = Lens.lens (\AddAttributesActivity' {next} -> next) (\s@AddAttributesActivity' {} a -> s {next = a} :: AddAttributesActivity)

-- | The name of the addAttributes activity.
addAttributesActivity_name :: Lens.Lens' AddAttributesActivity Prelude.Text
addAttributesActivity_name = Lens.lens (\AddAttributesActivity' {name} -> name) (\s@AddAttributesActivity' {} a -> s {name = a} :: AddAttributesActivity)

-- | A list of 1-50 @AttributeNameMapping@ objects that map an existing
-- attribute to a new attribute.
--
-- The existing attributes remain in the message, so if you want to remove
-- the originals, use @RemoveAttributeActivity@.
addAttributesActivity_attributes :: Lens.Lens' AddAttributesActivity (Prelude.HashMap Prelude.Text Prelude.Text)
addAttributesActivity_attributes = Lens.lens (\AddAttributesActivity' {attributes} -> attributes) (\s@AddAttributesActivity' {} a -> s {attributes = a} :: AddAttributesActivity) Prelude.. Lens.coerced

instance Core.FromJSON AddAttributesActivity where
  parseJSON =
    Core.withObject
      "AddAttributesActivity"
      ( \x ->
          AddAttributesActivity'
            Prelude.<$> (x Core..:? "next")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AddAttributesActivity where
  hashWithSalt _salt AddAttributesActivity' {..} =
    _salt `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData AddAttributesActivity where
  rnf AddAttributesActivity' {..} =
    Prelude.rnf next
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf attributes

instance Core.ToJSON AddAttributesActivity where
  toJSON AddAttributesActivity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("next" Core..=) Prelude.<$> next,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("attributes" Core..= attributes)
          ]
      )
