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
-- Module      : Network.AWS.IoTAnalytics.Types.AddAttributesActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.AddAttributesActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An activity that adds other attributes based on existing attributes in
-- the message.
--
-- /See:/ 'newAddAttributesActivity' smart constructor.
data AddAttributesActivity = AddAttributesActivity'
  { -- | The next activity in the pipeline.
    next :: Core.Maybe Core.Text,
    -- | The name of the addAttributes activity.
    name :: Core.Text,
    -- | A list of 1-50 @AttributeNameMapping@ objects that map an existing
    -- attribute to a new attribute.
    --
    -- The existing attributes remain in the message, so if you want to remove
    -- the originals, use @RemoveAttributeActivity@.
    attributes :: Core.HashMap Core.Text Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  AddAttributesActivity
newAddAttributesActivity pName_ =
  AddAttributesActivity'
    { next = Core.Nothing,
      name = pName_,
      attributes = Core.mempty
    }

-- | The next activity in the pipeline.
addAttributesActivity_next :: Lens.Lens' AddAttributesActivity (Core.Maybe Core.Text)
addAttributesActivity_next = Lens.lens (\AddAttributesActivity' {next} -> next) (\s@AddAttributesActivity' {} a -> s {next = a} :: AddAttributesActivity)

-- | The name of the addAttributes activity.
addAttributesActivity_name :: Lens.Lens' AddAttributesActivity Core.Text
addAttributesActivity_name = Lens.lens (\AddAttributesActivity' {name} -> name) (\s@AddAttributesActivity' {} a -> s {name = a} :: AddAttributesActivity)

-- | A list of 1-50 @AttributeNameMapping@ objects that map an existing
-- attribute to a new attribute.
--
-- The existing attributes remain in the message, so if you want to remove
-- the originals, use @RemoveAttributeActivity@.
addAttributesActivity_attributes :: Lens.Lens' AddAttributesActivity (Core.HashMap Core.Text Core.Text)
addAttributesActivity_attributes = Lens.lens (\AddAttributesActivity' {attributes} -> attributes) (\s@AddAttributesActivity' {} a -> s {attributes = a} :: AddAttributesActivity) Core.. Lens._Coerce

instance Core.FromJSON AddAttributesActivity where
  parseJSON =
    Core.withObject
      "AddAttributesActivity"
      ( \x ->
          AddAttributesActivity'
            Core.<$> (x Core..:? "next")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..:? "attributes" Core..!= Core.mempty)
      )

instance Core.Hashable AddAttributesActivity

instance Core.NFData AddAttributesActivity

instance Core.ToJSON AddAttributesActivity where
  toJSON AddAttributesActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("next" Core..=) Core.<$> next,
            Core.Just ("name" Core..= name),
            Core.Just ("attributes" Core..= attributes)
          ]
      )
