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
import qualified Network.AWS.Prelude as Prelude

-- | An activity that removes attributes from a message.
--
-- /See:/ 'newRemoveAttributesActivity' smart constructor.
data RemoveAttributesActivity = RemoveAttributesActivity'
  { -- | The next activity in the pipeline.
    next :: Prelude.Maybe Prelude.Text,
    -- | The name of the @removeAttributes@ activity.
    name :: Prelude.Text,
    -- | A list of 1-50 attributes to remove from the message.
    attributes :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'attributes'
  Prelude.NonEmpty Prelude.Text ->
  RemoveAttributesActivity
newRemoveAttributesActivity pName_ pAttributes_ =
  RemoveAttributesActivity'
    { next = Prelude.Nothing,
      name = pName_,
      attributes = Lens._Coerce Lens.# pAttributes_
    }

-- | The next activity in the pipeline.
removeAttributesActivity_next :: Lens.Lens' RemoveAttributesActivity (Prelude.Maybe Prelude.Text)
removeAttributesActivity_next = Lens.lens (\RemoveAttributesActivity' {next} -> next) (\s@RemoveAttributesActivity' {} a -> s {next = a} :: RemoveAttributesActivity)

-- | The name of the @removeAttributes@ activity.
removeAttributesActivity_name :: Lens.Lens' RemoveAttributesActivity Prelude.Text
removeAttributesActivity_name = Lens.lens (\RemoveAttributesActivity' {name} -> name) (\s@RemoveAttributesActivity' {} a -> s {name = a} :: RemoveAttributesActivity)

-- | A list of 1-50 attributes to remove from the message.
removeAttributesActivity_attributes :: Lens.Lens' RemoveAttributesActivity (Prelude.NonEmpty Prelude.Text)
removeAttributesActivity_attributes = Lens.lens (\RemoveAttributesActivity' {attributes} -> attributes) (\s@RemoveAttributesActivity' {} a -> s {attributes = a} :: RemoveAttributesActivity) Prelude.. Lens._Coerce

instance Core.FromJSON RemoveAttributesActivity where
  parseJSON =
    Core.withObject
      "RemoveAttributesActivity"
      ( \x ->
          RemoveAttributesActivity'
            Prelude.<$> (x Core..:? "next")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "attributes")
      )

instance Prelude.Hashable RemoveAttributesActivity

instance Prelude.NFData RemoveAttributesActivity

instance Core.ToJSON RemoveAttributesActivity where
  toJSON RemoveAttributesActivity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("next" Core..=) Prelude.<$> next,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("attributes" Core..= attributes)
          ]
      )
