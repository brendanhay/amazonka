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
-- Module      : Amazonka.IotTwinMaker.Types.FunctionResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.FunctionResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IotTwinMaker.Types.DataConnector
import Amazonka.IotTwinMaker.Types.Scope
import qualified Amazonka.Prelude as Prelude

-- | The function response.
--
-- /See:/ 'newFunctionResponse' smart constructor.
data FunctionResponse = FunctionResponse'
  { -- | The required properties of the function.
    requiredProperties :: Prelude.Maybe [Prelude.Text],
    -- | The data connector.
    implementedBy :: Prelude.Maybe DataConnector,
    -- | The scope of the function.
    scope :: Prelude.Maybe Scope,
    -- | Indicates whether this function is inherited.
    isInherited :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requiredProperties', 'functionResponse_requiredProperties' - The required properties of the function.
--
-- 'implementedBy', 'functionResponse_implementedBy' - The data connector.
--
-- 'scope', 'functionResponse_scope' - The scope of the function.
--
-- 'isInherited', 'functionResponse_isInherited' - Indicates whether this function is inherited.
newFunctionResponse ::
  FunctionResponse
newFunctionResponse =
  FunctionResponse'
    { requiredProperties =
        Prelude.Nothing,
      implementedBy = Prelude.Nothing,
      scope = Prelude.Nothing,
      isInherited = Prelude.Nothing
    }

-- | The required properties of the function.
functionResponse_requiredProperties :: Lens.Lens' FunctionResponse (Prelude.Maybe [Prelude.Text])
functionResponse_requiredProperties = Lens.lens (\FunctionResponse' {requiredProperties} -> requiredProperties) (\s@FunctionResponse' {} a -> s {requiredProperties = a} :: FunctionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The data connector.
functionResponse_implementedBy :: Lens.Lens' FunctionResponse (Prelude.Maybe DataConnector)
functionResponse_implementedBy = Lens.lens (\FunctionResponse' {implementedBy} -> implementedBy) (\s@FunctionResponse' {} a -> s {implementedBy = a} :: FunctionResponse)

-- | The scope of the function.
functionResponse_scope :: Lens.Lens' FunctionResponse (Prelude.Maybe Scope)
functionResponse_scope = Lens.lens (\FunctionResponse' {scope} -> scope) (\s@FunctionResponse' {} a -> s {scope = a} :: FunctionResponse)

-- | Indicates whether this function is inherited.
functionResponse_isInherited :: Lens.Lens' FunctionResponse (Prelude.Maybe Prelude.Bool)
functionResponse_isInherited = Lens.lens (\FunctionResponse' {isInherited} -> isInherited) (\s@FunctionResponse' {} a -> s {isInherited = a} :: FunctionResponse)

instance Core.FromJSON FunctionResponse where
  parseJSON =
    Core.withObject
      "FunctionResponse"
      ( \x ->
          FunctionResponse'
            Prelude.<$> ( x Core..:? "requiredProperties"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "implementedBy")
            Prelude.<*> (x Core..:? "scope")
            Prelude.<*> (x Core..:? "isInherited")
      )

instance Prelude.Hashable FunctionResponse where
  hashWithSalt _salt FunctionResponse' {..} =
    _salt `Prelude.hashWithSalt` requiredProperties
      `Prelude.hashWithSalt` implementedBy
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` isInherited

instance Prelude.NFData FunctionResponse where
  rnf FunctionResponse' {..} =
    Prelude.rnf requiredProperties
      `Prelude.seq` Prelude.rnf implementedBy
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf isInherited
