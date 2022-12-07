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
-- Module      : Amazonka.IotTwinMaker.Types.ComponentRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ComponentRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.ComponentPropertyGroupRequest
import Amazonka.IotTwinMaker.Types.PropertyRequest
import qualified Amazonka.Prelude as Prelude

-- | An object that sets information about a component type create or update
-- request.
--
-- /See:/ 'newComponentRequest' smart constructor.
data ComponentRequest = ComponentRequest'
  { -- | An object that maps strings to the properties to set in the component
    -- type. Each string in the mapping must be unique to this object.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyRequest),
    -- | The description of the component request.
    description :: Prelude.Maybe Prelude.Text,
    -- | The property groups.
    propertyGroups :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentPropertyGroupRequest),
    -- | The ID of the component type.
    componentTypeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'properties', 'componentRequest_properties' - An object that maps strings to the properties to set in the component
-- type. Each string in the mapping must be unique to this object.
--
-- 'description', 'componentRequest_description' - The description of the component request.
--
-- 'propertyGroups', 'componentRequest_propertyGroups' - The property groups.
--
-- 'componentTypeId', 'componentRequest_componentTypeId' - The ID of the component type.
newComponentRequest ::
  ComponentRequest
newComponentRequest =
  ComponentRequest'
    { properties = Prelude.Nothing,
      description = Prelude.Nothing,
      propertyGroups = Prelude.Nothing,
      componentTypeId = Prelude.Nothing
    }

-- | An object that maps strings to the properties to set in the component
-- type. Each string in the mapping must be unique to this object.
componentRequest_properties :: Lens.Lens' ComponentRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyRequest))
componentRequest_properties = Lens.lens (\ComponentRequest' {properties} -> properties) (\s@ComponentRequest' {} a -> s {properties = a} :: ComponentRequest) Prelude.. Lens.mapping Lens.coerced

-- | The description of the component request.
componentRequest_description :: Lens.Lens' ComponentRequest (Prelude.Maybe Prelude.Text)
componentRequest_description = Lens.lens (\ComponentRequest' {description} -> description) (\s@ComponentRequest' {} a -> s {description = a} :: ComponentRequest)

-- | The property groups.
componentRequest_propertyGroups :: Lens.Lens' ComponentRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentPropertyGroupRequest))
componentRequest_propertyGroups = Lens.lens (\ComponentRequest' {propertyGroups} -> propertyGroups) (\s@ComponentRequest' {} a -> s {propertyGroups = a} :: ComponentRequest) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the component type.
componentRequest_componentTypeId :: Lens.Lens' ComponentRequest (Prelude.Maybe Prelude.Text)
componentRequest_componentTypeId = Lens.lens (\ComponentRequest' {componentTypeId} -> componentTypeId) (\s@ComponentRequest' {} a -> s {componentTypeId = a} :: ComponentRequest)

instance Prelude.Hashable ComponentRequest where
  hashWithSalt _salt ComponentRequest' {..} =
    _salt `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` propertyGroups
      `Prelude.hashWithSalt` componentTypeId

instance Prelude.NFData ComponentRequest where
  rnf ComponentRequest' {..} =
    Prelude.rnf properties
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf propertyGroups
      `Prelude.seq` Prelude.rnf componentTypeId

instance Data.ToJSON ComponentRequest where
  toJSON ComponentRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("properties" Data..=) Prelude.<$> properties,
            ("description" Data..=) Prelude.<$> description,
            ("propertyGroups" Data..=)
              Prelude.<$> propertyGroups,
            ("componentTypeId" Data..=)
              Prelude.<$> componentTypeId
          ]
      )
