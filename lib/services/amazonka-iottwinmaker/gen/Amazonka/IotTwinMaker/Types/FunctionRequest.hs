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
-- Module      : Amazonka.IotTwinMaker.Types.FunctionRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.FunctionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types.DataConnector
import Amazonka.IotTwinMaker.Types.Scope
import qualified Amazonka.Prelude as Prelude

-- | The function request body.
--
-- /See:/ 'newFunctionRequest' smart constructor.
data FunctionRequest = FunctionRequest'
  { -- | The required properties of the function.
    requiredProperties :: Prelude.Maybe [Prelude.Text],
    -- | The data connector.
    implementedBy :: Prelude.Maybe DataConnector,
    -- | The scope of the function.
    scope :: Prelude.Maybe Scope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requiredProperties', 'functionRequest_requiredProperties' - The required properties of the function.
--
-- 'implementedBy', 'functionRequest_implementedBy' - The data connector.
--
-- 'scope', 'functionRequest_scope' - The scope of the function.
newFunctionRequest ::
  FunctionRequest
newFunctionRequest =
  FunctionRequest'
    { requiredProperties =
        Prelude.Nothing,
      implementedBy = Prelude.Nothing,
      scope = Prelude.Nothing
    }

-- | The required properties of the function.
functionRequest_requiredProperties :: Lens.Lens' FunctionRequest (Prelude.Maybe [Prelude.Text])
functionRequest_requiredProperties = Lens.lens (\FunctionRequest' {requiredProperties} -> requiredProperties) (\s@FunctionRequest' {} a -> s {requiredProperties = a} :: FunctionRequest) Prelude.. Lens.mapping Lens.coerced

-- | The data connector.
functionRequest_implementedBy :: Lens.Lens' FunctionRequest (Prelude.Maybe DataConnector)
functionRequest_implementedBy = Lens.lens (\FunctionRequest' {implementedBy} -> implementedBy) (\s@FunctionRequest' {} a -> s {implementedBy = a} :: FunctionRequest)

-- | The scope of the function.
functionRequest_scope :: Lens.Lens' FunctionRequest (Prelude.Maybe Scope)
functionRequest_scope = Lens.lens (\FunctionRequest' {scope} -> scope) (\s@FunctionRequest' {} a -> s {scope = a} :: FunctionRequest)

instance Prelude.Hashable FunctionRequest where
  hashWithSalt _salt FunctionRequest' {..} =
    _salt `Prelude.hashWithSalt` requiredProperties
      `Prelude.hashWithSalt` implementedBy
      `Prelude.hashWithSalt` scope

instance Prelude.NFData FunctionRequest where
  rnf FunctionRequest' {..} =
    Prelude.rnf requiredProperties
      `Prelude.seq` Prelude.rnf implementedBy
      `Prelude.seq` Prelude.rnf scope

instance Core.ToJSON FunctionRequest where
  toJSON FunctionRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("requiredProperties" Core..=)
              Prelude.<$> requiredProperties,
            ("implementedBy" Core..=) Prelude.<$> implementedBy,
            ("scope" Core..=) Prelude.<$> scope
          ]
      )
