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
-- Module      : Amazonka.IotTwinMaker.Types.ComponentResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ComponentResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.ComponentPropertyGroupResponse
import Amazonka.IotTwinMaker.Types.PropertyResponse
import Amazonka.IotTwinMaker.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | An object that returns information about a component type create or
-- update request.
--
-- /See:/ 'newComponentResponse' smart constructor.
data ComponentResponse = ComponentResponse'
  { -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the component type.
    componentTypeId :: Prelude.Maybe Prelude.Text,
    -- | The name of the property definition set in the request.
    definedIn :: Prelude.Maybe Prelude.Text,
    -- | The description of the component type.
    description :: Prelude.Maybe Prelude.Text,
    -- | An object that maps strings to the properties to set in the component
    -- type. Each string in the mapping must be unique to this object.
    properties :: Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyResponse),
    -- | The property groups.
    propertyGroups :: Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentPropertyGroupResponse),
    -- | The status of the component type.
    status :: Prelude.Maybe Status,
    -- | The syncSource of the sync job, if this entity was created by a sync
    -- job.
    syncSource :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentName', 'componentResponse_componentName' - The name of the component.
--
-- 'componentTypeId', 'componentResponse_componentTypeId' - The ID of the component type.
--
-- 'definedIn', 'componentResponse_definedIn' - The name of the property definition set in the request.
--
-- 'description', 'componentResponse_description' - The description of the component type.
--
-- 'properties', 'componentResponse_properties' - An object that maps strings to the properties to set in the component
-- type. Each string in the mapping must be unique to this object.
--
-- 'propertyGroups', 'componentResponse_propertyGroups' - The property groups.
--
-- 'status', 'componentResponse_status' - The status of the component type.
--
-- 'syncSource', 'componentResponse_syncSource' - The syncSource of the sync job, if this entity was created by a sync
-- job.
newComponentResponse ::
  ComponentResponse
newComponentResponse =
  ComponentResponse'
    { componentName = Prelude.Nothing,
      componentTypeId = Prelude.Nothing,
      definedIn = Prelude.Nothing,
      description = Prelude.Nothing,
      properties = Prelude.Nothing,
      propertyGroups = Prelude.Nothing,
      status = Prelude.Nothing,
      syncSource = Prelude.Nothing
    }

-- | The name of the component.
componentResponse_componentName :: Lens.Lens' ComponentResponse (Prelude.Maybe Prelude.Text)
componentResponse_componentName = Lens.lens (\ComponentResponse' {componentName} -> componentName) (\s@ComponentResponse' {} a -> s {componentName = a} :: ComponentResponse)

-- | The ID of the component type.
componentResponse_componentTypeId :: Lens.Lens' ComponentResponse (Prelude.Maybe Prelude.Text)
componentResponse_componentTypeId = Lens.lens (\ComponentResponse' {componentTypeId} -> componentTypeId) (\s@ComponentResponse' {} a -> s {componentTypeId = a} :: ComponentResponse)

-- | The name of the property definition set in the request.
componentResponse_definedIn :: Lens.Lens' ComponentResponse (Prelude.Maybe Prelude.Text)
componentResponse_definedIn = Lens.lens (\ComponentResponse' {definedIn} -> definedIn) (\s@ComponentResponse' {} a -> s {definedIn = a} :: ComponentResponse)

-- | The description of the component type.
componentResponse_description :: Lens.Lens' ComponentResponse (Prelude.Maybe Prelude.Text)
componentResponse_description = Lens.lens (\ComponentResponse' {description} -> description) (\s@ComponentResponse' {} a -> s {description = a} :: ComponentResponse)

-- | An object that maps strings to the properties to set in the component
-- type. Each string in the mapping must be unique to this object.
componentResponse_properties :: Lens.Lens' ComponentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text PropertyResponse))
componentResponse_properties = Lens.lens (\ComponentResponse' {properties} -> properties) (\s@ComponentResponse' {} a -> s {properties = a} :: ComponentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The property groups.
componentResponse_propertyGroups :: Lens.Lens' ComponentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ComponentPropertyGroupResponse))
componentResponse_propertyGroups = Lens.lens (\ComponentResponse' {propertyGroups} -> propertyGroups) (\s@ComponentResponse' {} a -> s {propertyGroups = a} :: ComponentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of the component type.
componentResponse_status :: Lens.Lens' ComponentResponse (Prelude.Maybe Status)
componentResponse_status = Lens.lens (\ComponentResponse' {status} -> status) (\s@ComponentResponse' {} a -> s {status = a} :: ComponentResponse)

-- | The syncSource of the sync job, if this entity was created by a sync
-- job.
componentResponse_syncSource :: Lens.Lens' ComponentResponse (Prelude.Maybe Prelude.Text)
componentResponse_syncSource = Lens.lens (\ComponentResponse' {syncSource} -> syncSource) (\s@ComponentResponse' {} a -> s {syncSource = a} :: ComponentResponse)

instance Data.FromJSON ComponentResponse where
  parseJSON =
    Data.withObject
      "ComponentResponse"
      ( \x ->
          ComponentResponse'
            Prelude.<$> (x Data..:? "componentName")
            Prelude.<*> (x Data..:? "componentTypeId")
            Prelude.<*> (x Data..:? "definedIn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "properties" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "propertyGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "syncSource")
      )

instance Prelude.Hashable ComponentResponse where
  hashWithSalt _salt ComponentResponse' {..} =
    _salt `Prelude.hashWithSalt` componentName
      `Prelude.hashWithSalt` componentTypeId
      `Prelude.hashWithSalt` definedIn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` propertyGroups
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` syncSource

instance Prelude.NFData ComponentResponse where
  rnf ComponentResponse' {..} =
    Prelude.rnf componentName
      `Prelude.seq` Prelude.rnf componentTypeId
      `Prelude.seq` Prelude.rnf definedIn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf propertyGroups
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf syncSource
