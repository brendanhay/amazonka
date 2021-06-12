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
-- Module      : Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An activity that removes attributes from a message.
--
-- /See:/ 'newRemoveAttributesActivity' smart constructor.
data RemoveAttributesActivity = RemoveAttributesActivity'
  { -- | The next activity in the pipeline.
    next :: Core.Maybe Core.Text,
    -- | The name of the @removeAttributes@ activity.
    name :: Core.Text,
    -- | A list of 1-50 attributes to remove from the message.
    attributes :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveAttributesActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'next', 'removeAttributesActivity_next' - The next activity in the pipeline.
--
-- 'name', 'removeAttributesActivity_name' - The name of the @removeAttributes@ activity.
--
-- 'attributes', 'removeAttributesActivity_attributes' - A list of 1-50 attributes to remove from the message.
newRemoveAttributesActivity ::
  -- | 'name'
  Core.Text ->
  -- | 'attributes'
  Core.NonEmpty Core.Text ->
  RemoveAttributesActivity
newRemoveAttributesActivity pName_ pAttributes_ =
  RemoveAttributesActivity'
    { next = Core.Nothing,
      name = pName_,
      attributes = Lens._Coerce Lens.# pAttributes_
    }

-- | The next activity in the pipeline.
removeAttributesActivity_next :: Lens.Lens' RemoveAttributesActivity (Core.Maybe Core.Text)
removeAttributesActivity_next = Lens.lens (\RemoveAttributesActivity' {next} -> next) (\s@RemoveAttributesActivity' {} a -> s {next = a} :: RemoveAttributesActivity)

-- | The name of the @removeAttributes@ activity.
removeAttributesActivity_name :: Lens.Lens' RemoveAttributesActivity Core.Text
removeAttributesActivity_name = Lens.lens (\RemoveAttributesActivity' {name} -> name) (\s@RemoveAttributesActivity' {} a -> s {name = a} :: RemoveAttributesActivity)

-- | A list of 1-50 attributes to remove from the message.
removeAttributesActivity_attributes :: Lens.Lens' RemoveAttributesActivity (Core.NonEmpty Core.Text)
removeAttributesActivity_attributes = Lens.lens (\RemoveAttributesActivity' {attributes} -> attributes) (\s@RemoveAttributesActivity' {} a -> s {attributes = a} :: RemoveAttributesActivity) Core.. Lens._Coerce

instance Core.FromJSON RemoveAttributesActivity where
  parseJSON =
    Core.withObject
      "RemoveAttributesActivity"
      ( \x ->
          RemoveAttributesActivity'
            Core.<$> (x Core..:? "next")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "attributes")
      )

instance Core.Hashable RemoveAttributesActivity

instance Core.NFData RemoveAttributesActivity

instance Core.ToJSON RemoveAttributesActivity where
  toJSON RemoveAttributesActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("next" Core..=) Core.<$> next,
            Core.Just ("name" Core..= name),
            Core.Just ("attributes" Core..= attributes)
          ]
      )
