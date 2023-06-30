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
-- Module      : Amazonka.IoTAnalytics.Types.RemoveAttributesActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.RemoveAttributesActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
      attributes = Lens.coerced Lens.# pAttributes_
    }

-- | The next activity in the pipeline.
removeAttributesActivity_next :: Lens.Lens' RemoveAttributesActivity (Prelude.Maybe Prelude.Text)
removeAttributesActivity_next = Lens.lens (\RemoveAttributesActivity' {next} -> next) (\s@RemoveAttributesActivity' {} a -> s {next = a} :: RemoveAttributesActivity)

-- | The name of the @removeAttributes@ activity.
removeAttributesActivity_name :: Lens.Lens' RemoveAttributesActivity Prelude.Text
removeAttributesActivity_name = Lens.lens (\RemoveAttributesActivity' {name} -> name) (\s@RemoveAttributesActivity' {} a -> s {name = a} :: RemoveAttributesActivity)

-- | A list of 1-50 attributes to remove from the message.
removeAttributesActivity_attributes :: Lens.Lens' RemoveAttributesActivity (Prelude.NonEmpty Prelude.Text)
removeAttributesActivity_attributes = Lens.lens (\RemoveAttributesActivity' {attributes} -> attributes) (\s@RemoveAttributesActivity' {} a -> s {attributes = a} :: RemoveAttributesActivity) Prelude.. Lens.coerced

instance Data.FromJSON RemoveAttributesActivity where
  parseJSON =
    Data.withObject
      "RemoveAttributesActivity"
      ( \x ->
          RemoveAttributesActivity'
            Prelude.<$> (x Data..:? "next")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "attributes")
      )

instance Prelude.Hashable RemoveAttributesActivity where
  hashWithSalt _salt RemoveAttributesActivity' {..} =
    _salt
      `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData RemoveAttributesActivity where
  rnf RemoveAttributesActivity' {..} =
    Prelude.rnf next
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf attributes

instance Data.ToJSON RemoveAttributesActivity where
  toJSON RemoveAttributesActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("next" Data..=) Prelude.<$> next,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("attributes" Data..= attributes)
          ]
      )
