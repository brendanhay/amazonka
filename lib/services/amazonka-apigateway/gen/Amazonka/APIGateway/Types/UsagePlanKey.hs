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
-- Module      : Amazonka.APIGateway.Types.UsagePlanKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.UsagePlanKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a usage plan key to identify a plan customer.
--
-- To associate an API stage with a selected API key in a usage plan, you
-- must create a UsagePlanKey resource to represent the selected ApiKey.
--
-- \"
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans>
--
-- /See:/ 'newUsagePlanKey' smart constructor.
data UsagePlanKey = UsagePlanKey'
  { -- | The value of a usage plan key.
    value :: Prelude.Maybe Prelude.Text,
    -- | The name of a usage plan key.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Id of a usage plan key.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of a usage plan key. Currently, the valid key type is
    -- @API_KEY@.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsagePlanKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'usagePlanKey_value' - The value of a usage plan key.
--
-- 'name', 'usagePlanKey_name' - The name of a usage plan key.
--
-- 'id', 'usagePlanKey_id' - The Id of a usage plan key.
--
-- 'type'', 'usagePlanKey_type' - The type of a usage plan key. Currently, the valid key type is
-- @API_KEY@.
newUsagePlanKey ::
  UsagePlanKey
newUsagePlanKey =
  UsagePlanKey'
    { value = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The value of a usage plan key.
usagePlanKey_value :: Lens.Lens' UsagePlanKey (Prelude.Maybe Prelude.Text)
usagePlanKey_value = Lens.lens (\UsagePlanKey' {value} -> value) (\s@UsagePlanKey' {} a -> s {value = a} :: UsagePlanKey)

-- | The name of a usage plan key.
usagePlanKey_name :: Lens.Lens' UsagePlanKey (Prelude.Maybe Prelude.Text)
usagePlanKey_name = Lens.lens (\UsagePlanKey' {name} -> name) (\s@UsagePlanKey' {} a -> s {name = a} :: UsagePlanKey)

-- | The Id of a usage plan key.
usagePlanKey_id :: Lens.Lens' UsagePlanKey (Prelude.Maybe Prelude.Text)
usagePlanKey_id = Lens.lens (\UsagePlanKey' {id} -> id) (\s@UsagePlanKey' {} a -> s {id = a} :: UsagePlanKey)

-- | The type of a usage plan key. Currently, the valid key type is
-- @API_KEY@.
usagePlanKey_type :: Lens.Lens' UsagePlanKey (Prelude.Maybe Prelude.Text)
usagePlanKey_type = Lens.lens (\UsagePlanKey' {type'} -> type') (\s@UsagePlanKey' {} a -> s {type' = a} :: UsagePlanKey)

instance Core.FromJSON UsagePlanKey where
  parseJSON =
    Core.withObject
      "UsagePlanKey"
      ( \x ->
          UsagePlanKey'
            Prelude.<$> (x Core..:? "value")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "type")
      )

instance Prelude.Hashable UsagePlanKey where
  hashWithSalt salt' UsagePlanKey' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData UsagePlanKey where
  rnf UsagePlanKey' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
