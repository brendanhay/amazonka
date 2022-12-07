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
-- Module      : Amazonka.IoTSiteWise.Types.VariableValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.VariableValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies a property value used in an expression.
--
-- /See:/ 'newVariableValue' smart constructor.
data VariableValue = VariableValue'
  { -- | The ID of the hierarchy to query for the property ID. You can use the
    -- hierarchy\'s name instead of the hierarchy\'s ID.
    --
    -- You use a hierarchy ID instead of a model ID because you can have
    -- several hierarchies using the same model and therefore the same
    -- @propertyId@. For example, you might have separately grouped assets that
    -- come from the same asset model. For more information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
    -- in the /IoT SiteWise User Guide/.
    hierarchyId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the property to use as the variable. You can use the property
    -- @name@ if it\'s from the same asset model.
    propertyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariableValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hierarchyId', 'variableValue_hierarchyId' - The ID of the hierarchy to query for the property ID. You can use the
-- hierarchy\'s name instead of the hierarchy\'s ID.
--
-- You use a hierarchy ID instead of a model ID because you can have
-- several hierarchies using the same model and therefore the same
-- @propertyId@. For example, you might have separately grouped assets that
-- come from the same asset model. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
-- in the /IoT SiteWise User Guide/.
--
-- 'propertyId', 'variableValue_propertyId' - The ID of the property to use as the variable. You can use the property
-- @name@ if it\'s from the same asset model.
newVariableValue ::
  -- | 'propertyId'
  Prelude.Text ->
  VariableValue
newVariableValue pPropertyId_ =
  VariableValue'
    { hierarchyId = Prelude.Nothing,
      propertyId = pPropertyId_
    }

-- | The ID of the hierarchy to query for the property ID. You can use the
-- hierarchy\'s name instead of the hierarchy\'s ID.
--
-- You use a hierarchy ID instead of a model ID because you can have
-- several hierarchies using the same model and therefore the same
-- @propertyId@. For example, you might have separately grouped assets that
-- come from the same asset model. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-hierarchies.html Asset hierarchies>
-- in the /IoT SiteWise User Guide/.
variableValue_hierarchyId :: Lens.Lens' VariableValue (Prelude.Maybe Prelude.Text)
variableValue_hierarchyId = Lens.lens (\VariableValue' {hierarchyId} -> hierarchyId) (\s@VariableValue' {} a -> s {hierarchyId = a} :: VariableValue)

-- | The ID of the property to use as the variable. You can use the property
-- @name@ if it\'s from the same asset model.
variableValue_propertyId :: Lens.Lens' VariableValue Prelude.Text
variableValue_propertyId = Lens.lens (\VariableValue' {propertyId} -> propertyId) (\s@VariableValue' {} a -> s {propertyId = a} :: VariableValue)

instance Data.FromJSON VariableValue where
  parseJSON =
    Data.withObject
      "VariableValue"
      ( \x ->
          VariableValue'
            Prelude.<$> (x Data..:? "hierarchyId")
            Prelude.<*> (x Data..: "propertyId")
      )

instance Prelude.Hashable VariableValue where
  hashWithSalt _salt VariableValue' {..} =
    _salt `Prelude.hashWithSalt` hierarchyId
      `Prelude.hashWithSalt` propertyId

instance Prelude.NFData VariableValue where
  rnf VariableValue' {..} =
    Prelude.rnf hierarchyId
      `Prelude.seq` Prelude.rnf propertyId

instance Data.ToJSON VariableValue where
  toJSON VariableValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("hierarchyId" Data..=) Prelude.<$> hierarchyId,
            Prelude.Just ("propertyId" Data..= propertyId)
          ]
      )
