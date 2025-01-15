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
-- Module      : Amazonka.IotTwinMaker.Types.PropertyResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.PropertyResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.DataValue
import Amazonka.IotTwinMaker.Types.PropertyDefinitionResponse
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about a property response.
--
-- /See:/ 'newPropertyResponse' smart constructor.
data PropertyResponse = PropertyResponse'
  { -- | An object that specifies information about a property.
    definition :: Prelude.Maybe PropertyDefinitionResponse,
    -- | The value of the property.
    value :: Prelude.Maybe DataValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'definition', 'propertyResponse_definition' - An object that specifies information about a property.
--
-- 'value', 'propertyResponse_value' - The value of the property.
newPropertyResponse ::
  PropertyResponse
newPropertyResponse =
  PropertyResponse'
    { definition = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | An object that specifies information about a property.
propertyResponse_definition :: Lens.Lens' PropertyResponse (Prelude.Maybe PropertyDefinitionResponse)
propertyResponse_definition = Lens.lens (\PropertyResponse' {definition} -> definition) (\s@PropertyResponse' {} a -> s {definition = a} :: PropertyResponse)

-- | The value of the property.
propertyResponse_value :: Lens.Lens' PropertyResponse (Prelude.Maybe DataValue)
propertyResponse_value = Lens.lens (\PropertyResponse' {value} -> value) (\s@PropertyResponse' {} a -> s {value = a} :: PropertyResponse)

instance Data.FromJSON PropertyResponse where
  parseJSON =
    Data.withObject
      "PropertyResponse"
      ( \x ->
          PropertyResponse'
            Prelude.<$> (x Data..:? "definition")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable PropertyResponse where
  hashWithSalt _salt PropertyResponse' {..} =
    _salt
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` value

instance Prelude.NFData PropertyResponse where
  rnf PropertyResponse' {..} =
    Prelude.rnf definition `Prelude.seq`
      Prelude.rnf value
