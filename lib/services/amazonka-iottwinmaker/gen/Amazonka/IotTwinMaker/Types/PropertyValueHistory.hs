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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyValueHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyValueHistory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types.EntityPropertyReference
import Amazonka.IotTwinMaker.Types.PropertyValue
import qualified Amazonka.Prelude as Prelude

-- | The history of values for a time series property.
--
-- /See:/ 'newPropertyValueHistory' smart constructor.
data PropertyValueHistory = PropertyValueHistory'
  { -- | A list of objects that contain information about the values in the
    -- history of a time series property.
    values :: Prelude.Maybe [PropertyValue],
    -- | An object that uniquely identifies an entity property.
    entityPropertyReference :: EntityPropertyReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyValueHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'propertyValueHistory_values' - A list of objects that contain information about the values in the
-- history of a time series property.
--
-- 'entityPropertyReference', 'propertyValueHistory_entityPropertyReference' - An object that uniquely identifies an entity property.
newPropertyValueHistory ::
  -- | 'entityPropertyReference'
  EntityPropertyReference ->
  PropertyValueHistory
newPropertyValueHistory pEntityPropertyReference_ =
  PropertyValueHistory'
    { values = Prelude.Nothing,
      entityPropertyReference = pEntityPropertyReference_
    }

-- | A list of objects that contain information about the values in the
-- history of a time series property.
propertyValueHistory_values :: Lens.Lens' PropertyValueHistory (Prelude.Maybe [PropertyValue])
propertyValueHistory_values = Lens.lens (\PropertyValueHistory' {values} -> values) (\s@PropertyValueHistory' {} a -> s {values = a} :: PropertyValueHistory) Prelude.. Lens.mapping Lens.coerced

-- | An object that uniquely identifies an entity property.
propertyValueHistory_entityPropertyReference :: Lens.Lens' PropertyValueHistory EntityPropertyReference
propertyValueHistory_entityPropertyReference = Lens.lens (\PropertyValueHistory' {entityPropertyReference} -> entityPropertyReference) (\s@PropertyValueHistory' {} a -> s {entityPropertyReference = a} :: PropertyValueHistory)

instance Core.FromJSON PropertyValueHistory where
  parseJSON =
    Core.withObject
      "PropertyValueHistory"
      ( \x ->
          PropertyValueHistory'
            Prelude.<$> (x Core..:? "values" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "entityPropertyReference")
      )

instance Prelude.Hashable PropertyValueHistory where
  hashWithSalt _salt PropertyValueHistory' {..} =
    _salt `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` entityPropertyReference

instance Prelude.NFData PropertyValueHistory where
  rnf PropertyValueHistory' {..} =
    Prelude.rnf values
      `Prelude.seq` Prelude.rnf entityPropertyReference
