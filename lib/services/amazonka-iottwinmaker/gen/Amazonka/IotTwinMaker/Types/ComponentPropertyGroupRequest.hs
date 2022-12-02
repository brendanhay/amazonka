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
-- Module      : Amazonka.IotTwinMaker.Types.ComponentPropertyGroupRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ComponentPropertyGroupRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.GroupType
import Amazonka.IotTwinMaker.Types.PropertyGroupUpdateType
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newComponentPropertyGroupRequest' smart constructor.
data ComponentPropertyGroupRequest = ComponentPropertyGroupRequest'
  { -- | The update type.
    updateType :: Prelude.Maybe PropertyGroupUpdateType,
    -- | The property names.
    propertyNames :: Prelude.Maybe [Prelude.Text],
    -- | The group type.
    groupType :: Prelude.Maybe GroupType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentPropertyGroupRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateType', 'componentPropertyGroupRequest_updateType' - The update type.
--
-- 'propertyNames', 'componentPropertyGroupRequest_propertyNames' - The property names.
--
-- 'groupType', 'componentPropertyGroupRequest_groupType' - The group type.
newComponentPropertyGroupRequest ::
  ComponentPropertyGroupRequest
newComponentPropertyGroupRequest =
  ComponentPropertyGroupRequest'
    { updateType =
        Prelude.Nothing,
      propertyNames = Prelude.Nothing,
      groupType = Prelude.Nothing
    }

-- | The update type.
componentPropertyGroupRequest_updateType :: Lens.Lens' ComponentPropertyGroupRequest (Prelude.Maybe PropertyGroupUpdateType)
componentPropertyGroupRequest_updateType = Lens.lens (\ComponentPropertyGroupRequest' {updateType} -> updateType) (\s@ComponentPropertyGroupRequest' {} a -> s {updateType = a} :: ComponentPropertyGroupRequest)

-- | The property names.
componentPropertyGroupRequest_propertyNames :: Lens.Lens' ComponentPropertyGroupRequest (Prelude.Maybe [Prelude.Text])
componentPropertyGroupRequest_propertyNames = Lens.lens (\ComponentPropertyGroupRequest' {propertyNames} -> propertyNames) (\s@ComponentPropertyGroupRequest' {} a -> s {propertyNames = a} :: ComponentPropertyGroupRequest) Prelude.. Lens.mapping Lens.coerced

-- | The group type.
componentPropertyGroupRequest_groupType :: Lens.Lens' ComponentPropertyGroupRequest (Prelude.Maybe GroupType)
componentPropertyGroupRequest_groupType = Lens.lens (\ComponentPropertyGroupRequest' {groupType} -> groupType) (\s@ComponentPropertyGroupRequest' {} a -> s {groupType = a} :: ComponentPropertyGroupRequest)

instance
  Prelude.Hashable
    ComponentPropertyGroupRequest
  where
  hashWithSalt _salt ComponentPropertyGroupRequest' {..} =
    _salt `Prelude.hashWithSalt` updateType
      `Prelude.hashWithSalt` propertyNames
      `Prelude.hashWithSalt` groupType

instance Prelude.NFData ComponentPropertyGroupRequest where
  rnf ComponentPropertyGroupRequest' {..} =
    Prelude.rnf updateType
      `Prelude.seq` Prelude.rnf propertyNames
      `Prelude.seq` Prelude.rnf groupType

instance Data.ToJSON ComponentPropertyGroupRequest where
  toJSON ComponentPropertyGroupRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("updateType" Data..=) Prelude.<$> updateType,
            ("propertyNames" Data..=) Prelude.<$> propertyNames,
            ("groupType" Data..=) Prelude.<$> groupType
          ]
      )
