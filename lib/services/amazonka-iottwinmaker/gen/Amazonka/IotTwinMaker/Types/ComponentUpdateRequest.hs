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
-- Module      : Amazonka.IotTwinMaker.Types.ComponentUpdateRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ComponentUpdateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.ComponentPropertyGroupRequest
import Amazonka.IotTwinMaker.Types.ComponentUpdateType
import Amazonka.IotTwinMaker.Types.PropertyRequest
import qualified Amazonka.Prelude as Prelude

-- | The component update request.
--
-- /See:/ 'newComponentUpdateRequest' smart constructor.
data ComponentUpdateRequest = ComponentUpdateRequest'
  { -- | The ID of the component type.
    componentTypeId :: Prelude.Maybe Prelude.Text,
    -- | The description of the component type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The property group updates.
    propertyGroupUpdates :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentPropertyGroupRequest),
    -- | An object that maps strings to the properties to set in the component
    -- type update. Each string in the mapping must be unique to this object.
    propertyUpdates :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyRequest),
    -- | The update type of the component update request.
    updateType :: Prelude.Maybe ComponentUpdateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentUpdateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentTypeId', 'componentUpdateRequest_componentTypeId' - The ID of the component type.
--
-- 'description', 'componentUpdateRequest_description' - The description of the component type.
--
-- 'propertyGroupUpdates', 'componentUpdateRequest_propertyGroupUpdates' - The property group updates.
--
-- 'propertyUpdates', 'componentUpdateRequest_propertyUpdates' - An object that maps strings to the properties to set in the component
-- type update. Each string in the mapping must be unique to this object.
--
-- 'updateType', 'componentUpdateRequest_updateType' - The update type of the component update request.
newComponentUpdateRequest ::
  ComponentUpdateRequest
newComponentUpdateRequest =
  ComponentUpdateRequest'
    { componentTypeId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      propertyGroupUpdates = Prelude.Nothing,
      propertyUpdates = Prelude.Nothing,
      updateType = Prelude.Nothing
    }

-- | The ID of the component type.
componentUpdateRequest_componentTypeId :: Lens.Lens' ComponentUpdateRequest (Prelude.Maybe Prelude.Text)
componentUpdateRequest_componentTypeId = Lens.lens (\ComponentUpdateRequest' {componentTypeId} -> componentTypeId) (\s@ComponentUpdateRequest' {} a -> s {componentTypeId = a} :: ComponentUpdateRequest)

-- | The description of the component type.
componentUpdateRequest_description :: Lens.Lens' ComponentUpdateRequest (Prelude.Maybe Prelude.Text)
componentUpdateRequest_description = Lens.lens (\ComponentUpdateRequest' {description} -> description) (\s@ComponentUpdateRequest' {} a -> s {description = a} :: ComponentUpdateRequest)

-- | The property group updates.
componentUpdateRequest_propertyGroupUpdates :: Lens.Lens' ComponentUpdateRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentPropertyGroupRequest))
componentUpdateRequest_propertyGroupUpdates = Lens.lens (\ComponentUpdateRequest' {propertyGroupUpdates} -> propertyGroupUpdates) (\s@ComponentUpdateRequest' {} a -> s {propertyGroupUpdates = a} :: ComponentUpdateRequest) Prelude.. Lens.mapping Lens.coerced

-- | An object that maps strings to the properties to set in the component
-- type update. Each string in the mapping must be unique to this object.
componentUpdateRequest_propertyUpdates :: Lens.Lens' ComponentUpdateRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyRequest))
componentUpdateRequest_propertyUpdates = Lens.lens (\ComponentUpdateRequest' {propertyUpdates} -> propertyUpdates) (\s@ComponentUpdateRequest' {} a -> s {propertyUpdates = a} :: ComponentUpdateRequest) Prelude.. Lens.mapping Lens.coerced

-- | The update type of the component update request.
componentUpdateRequest_updateType :: Lens.Lens' ComponentUpdateRequest (Prelude.Maybe ComponentUpdateType)
componentUpdateRequest_updateType = Lens.lens (\ComponentUpdateRequest' {updateType} -> updateType) (\s@ComponentUpdateRequest' {} a -> s {updateType = a} :: ComponentUpdateRequest)

instance Prelude.Hashable ComponentUpdateRequest where
  hashWithSalt _salt ComponentUpdateRequest' {..} =
    _salt
      `Prelude.hashWithSalt` componentTypeId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` propertyGroupUpdates
      `Prelude.hashWithSalt` propertyUpdates
      `Prelude.hashWithSalt` updateType

instance Prelude.NFData ComponentUpdateRequest where
  rnf ComponentUpdateRequest' {..} =
    Prelude.rnf componentTypeId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf propertyGroupUpdates
      `Prelude.seq` Prelude.rnf propertyUpdates
      `Prelude.seq` Prelude.rnf updateType

instance Data.ToJSON ComponentUpdateRequest where
  toJSON ComponentUpdateRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("componentTypeId" Data..=)
              Prelude.<$> componentTypeId,
            ("description" Data..=) Prelude.<$> description,
            ("propertyGroupUpdates" Data..=)
              Prelude.<$> propertyGroupUpdates,
            ("propertyUpdates" Data..=)
              Prelude.<$> propertyUpdates,
            ("updateType" Data..=) Prelude.<$> updateType
          ]
      )
