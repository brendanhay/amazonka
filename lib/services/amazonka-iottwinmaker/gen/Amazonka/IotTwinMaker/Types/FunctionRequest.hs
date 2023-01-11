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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.FunctionRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.DataConnector
import Amazonka.IotTwinMaker.Types.Scope
import qualified Amazonka.Prelude as Prelude

-- | The function request body.
--
-- /See:/ 'newFunctionRequest' smart constructor.
data FunctionRequest = FunctionRequest'
  { -- | The data connector.
    implementedBy :: Prelude.Maybe DataConnector,
    -- | The required properties of the function.
    requiredProperties :: Prelude.Maybe [Prelude.Text],
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
-- 'implementedBy', 'functionRequest_implementedBy' - The data connector.
--
-- 'requiredProperties', 'functionRequest_requiredProperties' - The required properties of the function.
--
-- 'scope', 'functionRequest_scope' - The scope of the function.
newFunctionRequest ::
  FunctionRequest
newFunctionRequest =
  FunctionRequest'
    { implementedBy = Prelude.Nothing,
      requiredProperties = Prelude.Nothing,
      scope = Prelude.Nothing
    }

-- | The data connector.
functionRequest_implementedBy :: Lens.Lens' FunctionRequest (Prelude.Maybe DataConnector)
functionRequest_implementedBy = Lens.lens (\FunctionRequest' {implementedBy} -> implementedBy) (\s@FunctionRequest' {} a -> s {implementedBy = a} :: FunctionRequest)

-- | The required properties of the function.
functionRequest_requiredProperties :: Lens.Lens' FunctionRequest (Prelude.Maybe [Prelude.Text])
functionRequest_requiredProperties = Lens.lens (\FunctionRequest' {requiredProperties} -> requiredProperties) (\s@FunctionRequest' {} a -> s {requiredProperties = a} :: FunctionRequest) Prelude.. Lens.mapping Lens.coerced

-- | The scope of the function.
functionRequest_scope :: Lens.Lens' FunctionRequest (Prelude.Maybe Scope)
functionRequest_scope = Lens.lens (\FunctionRequest' {scope} -> scope) (\s@FunctionRequest' {} a -> s {scope = a} :: FunctionRequest)

instance Prelude.Hashable FunctionRequest where
  hashWithSalt _salt FunctionRequest' {..} =
    _salt `Prelude.hashWithSalt` implementedBy
      `Prelude.hashWithSalt` requiredProperties
      `Prelude.hashWithSalt` scope

instance Prelude.NFData FunctionRequest where
  rnf FunctionRequest' {..} =
    Prelude.rnf implementedBy
      `Prelude.seq` Prelude.rnf requiredProperties
      `Prelude.seq` Prelude.rnf scope

instance Data.ToJSON FunctionRequest where
  toJSON FunctionRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("implementedBy" Data..=) Prelude.<$> implementedBy,
            ("requiredProperties" Data..=)
              Prelude.<$> requiredProperties,
            ("scope" Data..=) Prelude.<$> scope
          ]
      )
