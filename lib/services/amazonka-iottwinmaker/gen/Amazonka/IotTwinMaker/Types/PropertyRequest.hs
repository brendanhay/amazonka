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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.DataValue
import Amazonka.IotTwinMaker.Types.PropertyDefinitionRequest
import Amazonka.IotTwinMaker.Types.PropertyUpdateType
import qualified Amazonka.Prelude as Prelude

-- | An object that sets information about a property.
--
-- /See:/ 'newPropertyRequest' smart constructor.
data PropertyRequest = PropertyRequest'
  { -- | The update type of the update property request.
    updateType :: Prelude.Maybe PropertyUpdateType,
    -- | An object that specifies information about a property.
    definition :: Prelude.Maybe PropertyDefinitionRequest,
    -- | The value of the property.
    value :: Prelude.Maybe DataValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateType', 'propertyRequest_updateType' - The update type of the update property request.
--
-- 'definition', 'propertyRequest_definition' - An object that specifies information about a property.
--
-- 'value', 'propertyRequest_value' - The value of the property.
newPropertyRequest ::
  PropertyRequest
newPropertyRequest =
  PropertyRequest'
    { updateType = Prelude.Nothing,
      definition = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The update type of the update property request.
propertyRequest_updateType :: Lens.Lens' PropertyRequest (Prelude.Maybe PropertyUpdateType)
propertyRequest_updateType = Lens.lens (\PropertyRequest' {updateType} -> updateType) (\s@PropertyRequest' {} a -> s {updateType = a} :: PropertyRequest)

-- | An object that specifies information about a property.
propertyRequest_definition :: Lens.Lens' PropertyRequest (Prelude.Maybe PropertyDefinitionRequest)
propertyRequest_definition = Lens.lens (\PropertyRequest' {definition} -> definition) (\s@PropertyRequest' {} a -> s {definition = a} :: PropertyRequest)

-- | The value of the property.
propertyRequest_value :: Lens.Lens' PropertyRequest (Prelude.Maybe DataValue)
propertyRequest_value = Lens.lens (\PropertyRequest' {value} -> value) (\s@PropertyRequest' {} a -> s {value = a} :: PropertyRequest)

instance Prelude.Hashable PropertyRequest where
  hashWithSalt _salt PropertyRequest' {..} =
    _salt `Prelude.hashWithSalt` updateType
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` value

instance Prelude.NFData PropertyRequest where
  rnf PropertyRequest' {..} =
    Prelude.rnf updateType
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON PropertyRequest where
  toJSON PropertyRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("updateType" Data..=) Prelude.<$> updateType,
            ("definition" Data..=) Prelude.<$> definition,
            ("value" Data..=) Prelude.<$> value
          ]
      )
