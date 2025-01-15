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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.FunctionResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.DataConnector
import Amazonka.IotTwinMaker.Types.Scope
import qualified Amazonka.Prelude as Prelude

-- | The function response.
--
-- /See:/ 'newFunctionResponse' smart constructor.
data FunctionResponse = FunctionResponse'
  { -- | The data connector.
    implementedBy :: Prelude.Maybe DataConnector,
    -- | Indicates whether this function is inherited.
    isInherited :: Prelude.Maybe Prelude.Bool,
    -- | The required properties of the function.
    requiredProperties :: Prelude.Maybe [Prelude.Text],
    -- | The scope of the function.
    scope :: Prelude.Maybe Scope
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
-- 'implementedBy', 'functionResponse_implementedBy' - The data connector.
--
-- 'isInherited', 'functionResponse_isInherited' - Indicates whether this function is inherited.
--
-- 'requiredProperties', 'functionResponse_requiredProperties' - The required properties of the function.
--
-- 'scope', 'functionResponse_scope' - The scope of the function.
newFunctionResponse ::
  FunctionResponse
newFunctionResponse =
  FunctionResponse'
    { implementedBy = Prelude.Nothing,
      isInherited = Prelude.Nothing,
      requiredProperties = Prelude.Nothing,
      scope = Prelude.Nothing
    }

-- | The data connector.
functionResponse_implementedBy :: Lens.Lens' FunctionResponse (Prelude.Maybe DataConnector)
functionResponse_implementedBy = Lens.lens (\FunctionResponse' {implementedBy} -> implementedBy) (\s@FunctionResponse' {} a -> s {implementedBy = a} :: FunctionResponse)

-- | Indicates whether this function is inherited.
functionResponse_isInherited :: Lens.Lens' FunctionResponse (Prelude.Maybe Prelude.Bool)
functionResponse_isInherited = Lens.lens (\FunctionResponse' {isInherited} -> isInherited) (\s@FunctionResponse' {} a -> s {isInherited = a} :: FunctionResponse)

-- | The required properties of the function.
functionResponse_requiredProperties :: Lens.Lens' FunctionResponse (Prelude.Maybe [Prelude.Text])
functionResponse_requiredProperties = Lens.lens (\FunctionResponse' {requiredProperties} -> requiredProperties) (\s@FunctionResponse' {} a -> s {requiredProperties = a} :: FunctionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The scope of the function.
functionResponse_scope :: Lens.Lens' FunctionResponse (Prelude.Maybe Scope)
functionResponse_scope = Lens.lens (\FunctionResponse' {scope} -> scope) (\s@FunctionResponse' {} a -> s {scope = a} :: FunctionResponse)

instance Data.FromJSON FunctionResponse where
  parseJSON =
    Data.withObject
      "FunctionResponse"
      ( \x ->
          FunctionResponse'
            Prelude.<$> (x Data..:? "implementedBy")
            Prelude.<*> (x Data..:? "isInherited")
            Prelude.<*> ( x
                            Data..:? "requiredProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "scope")
      )

instance Prelude.Hashable FunctionResponse where
  hashWithSalt _salt FunctionResponse' {..} =
    _salt
      `Prelude.hashWithSalt` implementedBy
      `Prelude.hashWithSalt` isInherited
      `Prelude.hashWithSalt` requiredProperties
      `Prelude.hashWithSalt` scope

instance Prelude.NFData FunctionResponse where
  rnf FunctionResponse' {..} =
    Prelude.rnf implementedBy `Prelude.seq`
      Prelude.rnf isInherited `Prelude.seq`
        Prelude.rnf requiredProperties `Prelude.seq`
          Prelude.rnf scope
