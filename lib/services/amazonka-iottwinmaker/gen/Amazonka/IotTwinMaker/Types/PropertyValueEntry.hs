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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyValueEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyValueEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types.EntityPropertyReference
import Amazonka.IotTwinMaker.Types.PropertyValue
import qualified Amazonka.Prelude as Prelude

-- | An object that specifies information about time series property values.
-- This object is used and consumed by the
-- <https://docs.aws.amazon.com/iot-twinmaker/latest/apireference/API_BatchPutPropertyValues.html BatchPutPropertyValues>
-- action.
--
-- /See:/ 'newPropertyValueEntry' smart constructor.
data PropertyValueEntry = PropertyValueEntry'
  { -- | A list of objects that specify time series property values.
    propertyValues :: Prelude.Maybe (Prelude.NonEmpty PropertyValue),
    -- | An object that contains information about the entity that has the
    -- property.
    entityPropertyReference :: EntityPropertyReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyValueEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyValues', 'propertyValueEntry_propertyValues' - A list of objects that specify time series property values.
--
-- 'entityPropertyReference', 'propertyValueEntry_entityPropertyReference' - An object that contains information about the entity that has the
-- property.
newPropertyValueEntry ::
  -- | 'entityPropertyReference'
  EntityPropertyReference ->
  PropertyValueEntry
newPropertyValueEntry pEntityPropertyReference_ =
  PropertyValueEntry'
    { propertyValues =
        Prelude.Nothing,
      entityPropertyReference = pEntityPropertyReference_
    }

-- | A list of objects that specify time series property values.
propertyValueEntry_propertyValues :: Lens.Lens' PropertyValueEntry (Prelude.Maybe (Prelude.NonEmpty PropertyValue))
propertyValueEntry_propertyValues = Lens.lens (\PropertyValueEntry' {propertyValues} -> propertyValues) (\s@PropertyValueEntry' {} a -> s {propertyValues = a} :: PropertyValueEntry) Prelude.. Lens.mapping Lens.coerced

-- | An object that contains information about the entity that has the
-- property.
propertyValueEntry_entityPropertyReference :: Lens.Lens' PropertyValueEntry EntityPropertyReference
propertyValueEntry_entityPropertyReference = Lens.lens (\PropertyValueEntry' {entityPropertyReference} -> entityPropertyReference) (\s@PropertyValueEntry' {} a -> s {entityPropertyReference = a} :: PropertyValueEntry)

instance Core.FromJSON PropertyValueEntry where
  parseJSON =
    Core.withObject
      "PropertyValueEntry"
      ( \x ->
          PropertyValueEntry'
            Prelude.<$> (x Core..:? "propertyValues")
            Prelude.<*> (x Core..: "entityPropertyReference")
      )

instance Prelude.Hashable PropertyValueEntry where
  hashWithSalt _salt PropertyValueEntry' {..} =
    _salt `Prelude.hashWithSalt` propertyValues
      `Prelude.hashWithSalt` entityPropertyReference

instance Prelude.NFData PropertyValueEntry where
  rnf PropertyValueEntry' {..} =
    Prelude.rnf propertyValues
      `Prelude.seq` Prelude.rnf entityPropertyReference

instance Core.ToJSON PropertyValueEntry where
  toJSON PropertyValueEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("propertyValues" Core..=)
              Prelude.<$> propertyValues,
            Prelude.Just
              ( "entityPropertyReference"
                  Core..= entityPropertyReference
              )
          ]
      )
