{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Creates a new message using only the specified attributes from the
-- original message.
--
-- /See:/ 'newSelectAttributesActivity' smart constructor.
data SelectAttributesActivity = SelectAttributesActivity'
  { -- | The next activity in the pipeline.
    next :: Prelude.Maybe Prelude.Text,
    -- | The name of the @selectAttributes@ activity.
    name :: Prelude.Text,
    -- | A list of the attributes to select from the message.
    attributes :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'attributes'
  Prelude.NonEmpty Prelude.Text ->
  SelectAttributesActivity
newSelectAttributesActivity pName_ pAttributes_ =
  SelectAttributesActivity'
    { next = Prelude.Nothing,
      name = pName_,
      attributes = Prelude._Coerce Lens.# pAttributes_
    }

-- | The next activity in the pipeline.
selectAttributesActivity_next :: Lens.Lens' SelectAttributesActivity (Prelude.Maybe Prelude.Text)
selectAttributesActivity_next = Lens.lens (\SelectAttributesActivity' {next} -> next) (\s@SelectAttributesActivity' {} a -> s {next = a} :: SelectAttributesActivity)

-- | The name of the @selectAttributes@ activity.
selectAttributesActivity_name :: Lens.Lens' SelectAttributesActivity Prelude.Text
selectAttributesActivity_name = Lens.lens (\SelectAttributesActivity' {name} -> name) (\s@SelectAttributesActivity' {} a -> s {name = a} :: SelectAttributesActivity)

-- | A list of the attributes to select from the message.
selectAttributesActivity_attributes :: Lens.Lens' SelectAttributesActivity (Prelude.NonEmpty Prelude.Text)
selectAttributesActivity_attributes = Lens.lens (\SelectAttributesActivity' {attributes} -> attributes) (\s@SelectAttributesActivity' {} a -> s {attributes = a} :: SelectAttributesActivity) Prelude.. Prelude._Coerce

instance Prelude.FromJSON SelectAttributesActivity where
  parseJSON =
    Prelude.withObject
      "SelectAttributesActivity"
      ( \x ->
          SelectAttributesActivity'
            Prelude.<$> (x Prelude..:? "next")
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "attributes")
      )

instance Prelude.Hashable SelectAttributesActivity

instance Prelude.NFData SelectAttributesActivity

instance Prelude.ToJSON SelectAttributesActivity where
  toJSON SelectAttributesActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("next" Prelude..=) Prelude.<$> next,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("attributes" Prelude..= attributes)
          ]
      )
