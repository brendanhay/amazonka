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
-- Module      : Amazonka.IoT.Types.AttributePayload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AttributePayload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The attribute payload.
--
-- /See:/ 'newAttributePayload' smart constructor.
data AttributePayload = AttributePayload'
  { -- | A JSON string containing up to three key-value pair in JSON format. For
    -- example:
    --
    -- @{\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}@
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether the list of attributes provided in the
    -- @AttributePayload@ is merged with the attributes stored in the registry,
    -- instead of overwriting them.
    --
    -- To remove an attribute, call @UpdateThing@ with an empty attribute
    -- value.
    --
    -- The @merge@ attribute is only valid when calling @UpdateThing@ or
    -- @UpdateThingGroup@.
    merge :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributePayload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'attributePayload_attributes' - A JSON string containing up to three key-value pair in JSON format. For
-- example:
--
-- @{\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}@
--
-- 'merge', 'attributePayload_merge' - Specifies whether the list of attributes provided in the
-- @AttributePayload@ is merged with the attributes stored in the registry,
-- instead of overwriting them.
--
-- To remove an attribute, call @UpdateThing@ with an empty attribute
-- value.
--
-- The @merge@ attribute is only valid when calling @UpdateThing@ or
-- @UpdateThingGroup@.
newAttributePayload ::
  AttributePayload
newAttributePayload =
  AttributePayload'
    { attributes = Prelude.Nothing,
      merge = Prelude.Nothing
    }

-- | A JSON string containing up to three key-value pair in JSON format. For
-- example:
--
-- @{\\\"attributes\\\":{\\\"string1\\\":\\\"string2\\\"}}@
attributePayload_attributes :: Lens.Lens' AttributePayload (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
attributePayload_attributes = Lens.lens (\AttributePayload' {attributes} -> attributes) (\s@AttributePayload' {} a -> s {attributes = a} :: AttributePayload) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the list of attributes provided in the
-- @AttributePayload@ is merged with the attributes stored in the registry,
-- instead of overwriting them.
--
-- To remove an attribute, call @UpdateThing@ with an empty attribute
-- value.
--
-- The @merge@ attribute is only valid when calling @UpdateThing@ or
-- @UpdateThingGroup@.
attributePayload_merge :: Lens.Lens' AttributePayload (Prelude.Maybe Prelude.Bool)
attributePayload_merge = Lens.lens (\AttributePayload' {merge} -> merge) (\s@AttributePayload' {} a -> s {merge = a} :: AttributePayload)

instance Data.FromJSON AttributePayload where
  parseJSON =
    Data.withObject
      "AttributePayload"
      ( \x ->
          AttributePayload'
            Prelude.<$> (x Data..:? "attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "merge")
      )

instance Prelude.Hashable AttributePayload where
  hashWithSalt _salt AttributePayload' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` merge

instance Prelude.NFData AttributePayload where
  rnf AttributePayload' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf merge

instance Data.ToJSON AttributePayload where
  toJSON AttributePayload' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributes" Data..=) Prelude.<$> attributes,
            ("merge" Data..=) Prelude.<$> merge
          ]
      )
