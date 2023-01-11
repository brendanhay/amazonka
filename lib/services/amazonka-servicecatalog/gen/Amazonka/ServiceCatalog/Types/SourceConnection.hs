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
-- Module      : Amazonka.ServiceCatalog.Types.SourceConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.SourceConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.SourceConnectionParameters
import Amazonka.ServiceCatalog.Types.SourceType

-- | A top level @ProductViewDetail@ response containing details about the
-- product’s connection. Service Catalog returns this field for the
-- @CreateProduct@, @UpdateProduct@, @DescribeProductAsAdmin@, and
-- @SearchProductAsAdmin@ APIs. This response contains the same fields as
-- the @ConnectionParameters@ request, with the addition of the @LastSync@
-- response.
--
-- /See:/ 'newSourceConnection' smart constructor.
data SourceConnection = SourceConnection'
  { -- | The only supported @SourceConnection@ type is Codestar.
    type' :: Prelude.Maybe SourceType,
    -- | The connection details based on the connection @Type@.
    connectionParameters :: SourceConnectionParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'sourceConnection_type' - The only supported @SourceConnection@ type is Codestar.
--
-- 'connectionParameters', 'sourceConnection_connectionParameters' - The connection details based on the connection @Type@.
newSourceConnection ::
  -- | 'connectionParameters'
  SourceConnectionParameters ->
  SourceConnection
newSourceConnection pConnectionParameters_ =
  SourceConnection'
    { type' = Prelude.Nothing,
      connectionParameters = pConnectionParameters_
    }

-- | The only supported @SourceConnection@ type is Codestar.
sourceConnection_type :: Lens.Lens' SourceConnection (Prelude.Maybe SourceType)
sourceConnection_type = Lens.lens (\SourceConnection' {type'} -> type') (\s@SourceConnection' {} a -> s {type' = a} :: SourceConnection)

-- | The connection details based on the connection @Type@.
sourceConnection_connectionParameters :: Lens.Lens' SourceConnection SourceConnectionParameters
sourceConnection_connectionParameters = Lens.lens (\SourceConnection' {connectionParameters} -> connectionParameters) (\s@SourceConnection' {} a -> s {connectionParameters = a} :: SourceConnection)

instance Prelude.Hashable SourceConnection where
  hashWithSalt _salt SourceConnection' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` connectionParameters

instance Prelude.NFData SourceConnection where
  rnf SourceConnection' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf connectionParameters

instance Data.ToJSON SourceConnection where
  toJSON SourceConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Type" Data..=) Prelude.<$> type',
            Prelude.Just
              ( "ConnectionParameters"
                  Data..= connectionParameters
              )
          ]
      )
