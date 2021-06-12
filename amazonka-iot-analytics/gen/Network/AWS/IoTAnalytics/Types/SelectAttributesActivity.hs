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
-- Module      : Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.SelectAttributesActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Creates a new message using only the specified attributes from the
-- original message.
--
-- /See:/ 'newSelectAttributesActivity' smart constructor.
data SelectAttributesActivity = SelectAttributesActivity'
  { -- | The next activity in the pipeline.
    next :: Core.Maybe Core.Text,
    -- | The name of the @selectAttributes@ activity.
    name :: Core.Text,
    -- | A list of the attributes to select from the message.
    attributes :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SelectAttributesActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'next', 'selectAttributesActivity_next' - The next activity in the pipeline.
--
-- 'name', 'selectAttributesActivity_name' - The name of the @selectAttributes@ activity.
--
-- 'attributes', 'selectAttributesActivity_attributes' - A list of the attributes to select from the message.
newSelectAttributesActivity ::
  -- | 'name'
  Core.Text ->
  -- | 'attributes'
  Core.NonEmpty Core.Text ->
  SelectAttributesActivity
newSelectAttributesActivity pName_ pAttributes_ =
  SelectAttributesActivity'
    { next = Core.Nothing,
      name = pName_,
      attributes = Lens._Coerce Lens.# pAttributes_
    }

-- | The next activity in the pipeline.
selectAttributesActivity_next :: Lens.Lens' SelectAttributesActivity (Core.Maybe Core.Text)
selectAttributesActivity_next = Lens.lens (\SelectAttributesActivity' {next} -> next) (\s@SelectAttributesActivity' {} a -> s {next = a} :: SelectAttributesActivity)

-- | The name of the @selectAttributes@ activity.
selectAttributesActivity_name :: Lens.Lens' SelectAttributesActivity Core.Text
selectAttributesActivity_name = Lens.lens (\SelectAttributesActivity' {name} -> name) (\s@SelectAttributesActivity' {} a -> s {name = a} :: SelectAttributesActivity)

-- | A list of the attributes to select from the message.
selectAttributesActivity_attributes :: Lens.Lens' SelectAttributesActivity (Core.NonEmpty Core.Text)
selectAttributesActivity_attributes = Lens.lens (\SelectAttributesActivity' {attributes} -> attributes) (\s@SelectAttributesActivity' {} a -> s {attributes = a} :: SelectAttributesActivity) Core.. Lens._Coerce

instance Core.FromJSON SelectAttributesActivity where
  parseJSON =
    Core.withObject
      "SelectAttributesActivity"
      ( \x ->
          SelectAttributesActivity'
            Core.<$> (x Core..:? "next")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "attributes")
      )

instance Core.Hashable SelectAttributesActivity

instance Core.NFData SelectAttributesActivity

instance Core.ToJSON SelectAttributesActivity where
  toJSON SelectAttributesActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("next" Core..=) Core.<$> next,
            Core.Just ("name" Core..= name),
            Core.Just ("attributes" Core..= attributes)
          ]
      )
