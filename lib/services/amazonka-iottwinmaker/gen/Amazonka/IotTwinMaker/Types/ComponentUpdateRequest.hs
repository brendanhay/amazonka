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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ComponentUpdateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types.ComponentPropertyGroupRequest
import Amazonka.IotTwinMaker.Types.ComponentUpdateType
import Amazonka.IotTwinMaker.Types.PropertyRequest
import qualified Amazonka.Prelude as Prelude

-- | The component update request.
--
-- /See:/ 'newComponentUpdateRequest' smart constructor.
data ComponentUpdateRequest = ComponentUpdateRequest'
  { -- | The update type of the component update request.
    updateType :: Prelude.Maybe ComponentUpdateType,
    -- | An object that maps strings to the properties to set in the component
    -- type update. Each string in the mapping must be unique to this object.
    propertyUpdates :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyRequest),
    -- | The property group updates.
    propertyGroupUpdates :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentPropertyGroupRequest),
    -- | The description of the component type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the component type.
    componentTypeId :: Prelude.Maybe Prelude.Text
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
-- 'updateType', 'componentUpdateRequest_updateType' - The update type of the component update request.
--
-- 'propertyUpdates', 'componentUpdateRequest_propertyUpdates' - An object that maps strings to the properties to set in the component
-- type update. Each string in the mapping must be unique to this object.
--
-- 'propertyGroupUpdates', 'componentUpdateRequest_propertyGroupUpdates' - The property group updates.
--
-- 'description', 'componentUpdateRequest_description' - The description of the component type.
--
-- 'componentTypeId', 'componentUpdateRequest_componentTypeId' - The ID of the component type.
newComponentUpdateRequest ::
  ComponentUpdateRequest
newComponentUpdateRequest =
  ComponentUpdateRequest'
    { updateType =
        Prelude.Nothing,
      propertyUpdates = Prelude.Nothing,
      propertyGroupUpdates = Prelude.Nothing,
      description = Prelude.Nothing,
      componentTypeId = Prelude.Nothing
    }

-- | The update type of the component update request.
componentUpdateRequest_updateType :: Lens.Lens' ComponentUpdateRequest (Prelude.Maybe ComponentUpdateType)
componentUpdateRequest_updateType = Lens.lens (\ComponentUpdateRequest' {updateType} -> updateType) (\s@ComponentUpdateRequest' {} a -> s {updateType = a} :: ComponentUpdateRequest)

-- | An object that maps strings to the properties to set in the component
-- type update. Each string in the mapping must be unique to this object.
componentUpdateRequest_propertyUpdates :: Lens.Lens' ComponentUpdateRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyRequest))
componentUpdateRequest_propertyUpdates = Lens.lens (\ComponentUpdateRequest' {propertyUpdates} -> propertyUpdates) (\s@ComponentUpdateRequest' {} a -> s {propertyUpdates = a} :: ComponentUpdateRequest) Prelude.. Lens.mapping Lens.coerced

-- | The property group updates.
componentUpdateRequest_propertyGroupUpdates :: Lens.Lens' ComponentUpdateRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentPropertyGroupRequest))
componentUpdateRequest_propertyGroupUpdates = Lens.lens (\ComponentUpdateRequest' {propertyGroupUpdates} -> propertyGroupUpdates) (\s@ComponentUpdateRequest' {} a -> s {propertyGroupUpdates = a} :: ComponentUpdateRequest) Prelude.. Lens.mapping Lens.coerced

-- | The description of the component type.
componentUpdateRequest_description :: Lens.Lens' ComponentUpdateRequest (Prelude.Maybe Prelude.Text)
componentUpdateRequest_description = Lens.lens (\ComponentUpdateRequest' {description} -> description) (\s@ComponentUpdateRequest' {} a -> s {description = a} :: ComponentUpdateRequest)

-- | The ID of the component type.
componentUpdateRequest_componentTypeId :: Lens.Lens' ComponentUpdateRequest (Prelude.Maybe Prelude.Text)
componentUpdateRequest_componentTypeId = Lens.lens (\ComponentUpdateRequest' {componentTypeId} -> componentTypeId) (\s@ComponentUpdateRequest' {} a -> s {componentTypeId = a} :: ComponentUpdateRequest)

instance Prelude.Hashable ComponentUpdateRequest where
  hashWithSalt _salt ComponentUpdateRequest' {..} =
    _salt `Prelude.hashWithSalt` updateType
      `Prelude.hashWithSalt` propertyUpdates
      `Prelude.hashWithSalt` propertyGroupUpdates
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` componentTypeId

instance Prelude.NFData ComponentUpdateRequest where
  rnf ComponentUpdateRequest' {..} =
    Prelude.rnf updateType
      `Prelude.seq` Prelude.rnf propertyUpdates
      `Prelude.seq` Prelude.rnf propertyGroupUpdates
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf componentTypeId

instance Core.ToJSON ComponentUpdateRequest where
  toJSON ComponentUpdateRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("updateType" Core..=) Prelude.<$> updateType,
            ("propertyUpdates" Core..=)
              Prelude.<$> propertyUpdates,
            ("propertyGroupUpdates" Core..=)
              Prelude.<$> propertyGroupUpdates,
            ("description" Core..=) Prelude.<$> description,
            ("componentTypeId" Core..=)
              Prelude.<$> componentTypeId
          ]
      )
