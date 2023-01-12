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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyLatestValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyLatestValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.DataValue
import Amazonka.IotTwinMaker.Types.EntityPropertyReference
import qualified Amazonka.Prelude as Prelude

-- | The latest value of the property.
--
-- /See:/ 'newPropertyLatestValue' smart constructor.
data PropertyLatestValue = PropertyLatestValue'
  { -- | The value of the property.
    propertyValue :: Prelude.Maybe DataValue,
    -- | An object that specifies information about a property.>
    propertyReference :: EntityPropertyReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyLatestValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyValue', 'propertyLatestValue_propertyValue' - The value of the property.
--
-- 'propertyReference', 'propertyLatestValue_propertyReference' - An object that specifies information about a property.>
newPropertyLatestValue ::
  -- | 'propertyReference'
  EntityPropertyReference ->
  PropertyLatestValue
newPropertyLatestValue pPropertyReference_ =
  PropertyLatestValue'
    { propertyValue =
        Prelude.Nothing,
      propertyReference = pPropertyReference_
    }

-- | The value of the property.
propertyLatestValue_propertyValue :: Lens.Lens' PropertyLatestValue (Prelude.Maybe DataValue)
propertyLatestValue_propertyValue = Lens.lens (\PropertyLatestValue' {propertyValue} -> propertyValue) (\s@PropertyLatestValue' {} a -> s {propertyValue = a} :: PropertyLatestValue)

-- | An object that specifies information about a property.>
propertyLatestValue_propertyReference :: Lens.Lens' PropertyLatestValue EntityPropertyReference
propertyLatestValue_propertyReference = Lens.lens (\PropertyLatestValue' {propertyReference} -> propertyReference) (\s@PropertyLatestValue' {} a -> s {propertyReference = a} :: PropertyLatestValue)

instance Data.FromJSON PropertyLatestValue where
  parseJSON =
    Data.withObject
      "PropertyLatestValue"
      ( \x ->
          PropertyLatestValue'
            Prelude.<$> (x Data..:? "propertyValue")
            Prelude.<*> (x Data..: "propertyReference")
      )

instance Prelude.Hashable PropertyLatestValue where
  hashWithSalt _salt PropertyLatestValue' {..} =
    _salt `Prelude.hashWithSalt` propertyValue
      `Prelude.hashWithSalt` propertyReference

instance Prelude.NFData PropertyLatestValue where
  rnf PropertyLatestValue' {..} =
    Prelude.rnf propertyValue
      `Prelude.seq` Prelude.rnf propertyReference
