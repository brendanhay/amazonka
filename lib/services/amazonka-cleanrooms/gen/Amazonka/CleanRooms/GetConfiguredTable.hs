{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CleanRooms.GetConfiguredTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a configured table.
module Amazonka.CleanRooms.GetConfiguredTable
  ( -- * Creating a Request
    GetConfiguredTable (..),
    newGetConfiguredTable,

    -- * Request Lenses
    getConfiguredTable_configuredTableIdentifier,

    -- * Destructuring the Response
    GetConfiguredTableResponse (..),
    newGetConfiguredTableResponse,

    -- * Response Lenses
    getConfiguredTableResponse_httpStatus,
    getConfiguredTableResponse_configuredTable,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConfiguredTable' smart constructor.
data GetConfiguredTable = GetConfiguredTable'
  { -- | The unique ID for the configured table to retrieve.
    configuredTableIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfiguredTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredTableIdentifier', 'getConfiguredTable_configuredTableIdentifier' - The unique ID for the configured table to retrieve.
newGetConfiguredTable ::
  -- | 'configuredTableIdentifier'
  Prelude.Text ->
  GetConfiguredTable
newGetConfiguredTable pConfiguredTableIdentifier_ =
  GetConfiguredTable'
    { configuredTableIdentifier =
        pConfiguredTableIdentifier_
    }

-- | The unique ID for the configured table to retrieve.
getConfiguredTable_configuredTableIdentifier :: Lens.Lens' GetConfiguredTable Prelude.Text
getConfiguredTable_configuredTableIdentifier = Lens.lens (\GetConfiguredTable' {configuredTableIdentifier} -> configuredTableIdentifier) (\s@GetConfiguredTable' {} a -> s {configuredTableIdentifier = a} :: GetConfiguredTable)

instance Core.AWSRequest GetConfiguredTable where
  type
    AWSResponse GetConfiguredTable =
      GetConfiguredTableResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfiguredTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "configuredTable")
      )

instance Prelude.Hashable GetConfiguredTable where
  hashWithSalt _salt GetConfiguredTable' {..} =
    _salt
      `Prelude.hashWithSalt` configuredTableIdentifier

instance Prelude.NFData GetConfiguredTable where
  rnf GetConfiguredTable' {..} =
    Prelude.rnf configuredTableIdentifier

instance Data.ToHeaders GetConfiguredTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConfiguredTable where
  toPath GetConfiguredTable' {..} =
    Prelude.mconcat
      [ "/configuredTables/",
        Data.toBS configuredTableIdentifier
      ]

instance Data.ToQuery GetConfiguredTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConfiguredTableResponse' smart constructor.
data GetConfiguredTableResponse = GetConfiguredTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The retrieved configured table.
    configuredTable :: ConfiguredTable
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfiguredTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getConfiguredTableResponse_httpStatus' - The response's http status code.
--
-- 'configuredTable', 'getConfiguredTableResponse_configuredTable' - The retrieved configured table.
newGetConfiguredTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'configuredTable'
  ConfiguredTable ->
  GetConfiguredTableResponse
newGetConfiguredTableResponse
  pHttpStatus_
  pConfiguredTable_ =
    GetConfiguredTableResponse'
      { httpStatus =
          pHttpStatus_,
        configuredTable = pConfiguredTable_
      }

-- | The response's http status code.
getConfiguredTableResponse_httpStatus :: Lens.Lens' GetConfiguredTableResponse Prelude.Int
getConfiguredTableResponse_httpStatus = Lens.lens (\GetConfiguredTableResponse' {httpStatus} -> httpStatus) (\s@GetConfiguredTableResponse' {} a -> s {httpStatus = a} :: GetConfiguredTableResponse)

-- | The retrieved configured table.
getConfiguredTableResponse_configuredTable :: Lens.Lens' GetConfiguredTableResponse ConfiguredTable
getConfiguredTableResponse_configuredTable = Lens.lens (\GetConfiguredTableResponse' {configuredTable} -> configuredTable) (\s@GetConfiguredTableResponse' {} a -> s {configuredTable = a} :: GetConfiguredTableResponse)

instance Prelude.NFData GetConfiguredTableResponse where
  rnf GetConfiguredTableResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuredTable
