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
-- Module      : Amazonka.MigrationHubStrategy.GetServerStrategies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves recommended strategies and tools for the specified server.
module Amazonka.MigrationHubStrategy.GetServerStrategies
  ( -- * Creating a Request
    GetServerStrategies (..),
    newGetServerStrategies,

    -- * Request Lenses
    getServerStrategies_serverId,

    -- * Destructuring the Response
    GetServerStrategiesResponse (..),
    newGetServerStrategiesResponse,

    -- * Response Lenses
    getServerStrategiesResponse_serverStrategies,
    getServerStrategiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServerStrategies' smart constructor.
data GetServerStrategies = GetServerStrategies'
  { -- | The ID of the server.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServerStrategies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverId', 'getServerStrategies_serverId' - The ID of the server.
newGetServerStrategies ::
  -- | 'serverId'
  Prelude.Text ->
  GetServerStrategies
newGetServerStrategies pServerId_ =
  GetServerStrategies' {serverId = pServerId_}

-- | The ID of the server.
getServerStrategies_serverId :: Lens.Lens' GetServerStrategies Prelude.Text
getServerStrategies_serverId = Lens.lens (\GetServerStrategies' {serverId} -> serverId) (\s@GetServerStrategies' {} a -> s {serverId = a} :: GetServerStrategies)

instance Core.AWSRequest GetServerStrategies where
  type
    AWSResponse GetServerStrategies =
      GetServerStrategiesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServerStrategiesResponse'
            Prelude.<$> ( x
                            Data..?> "serverStrategies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServerStrategies where
  hashWithSalt _salt GetServerStrategies' {..} =
    _salt `Prelude.hashWithSalt` serverId

instance Prelude.NFData GetServerStrategies where
  rnf GetServerStrategies' {..} = Prelude.rnf serverId

instance Data.ToHeaders GetServerStrategies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetServerStrategies where
  toPath GetServerStrategies' {..} =
    Prelude.mconcat
      ["/get-server-strategies/", Data.toBS serverId]

instance Data.ToQuery GetServerStrategies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServerStrategiesResponse' smart constructor.
data GetServerStrategiesResponse = GetServerStrategiesResponse'
  { -- | A list of strategy recommendations for the server.
    serverStrategies :: Prelude.Maybe [ServerStrategy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServerStrategiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverStrategies', 'getServerStrategiesResponse_serverStrategies' - A list of strategy recommendations for the server.
--
-- 'httpStatus', 'getServerStrategiesResponse_httpStatus' - The response's http status code.
newGetServerStrategiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServerStrategiesResponse
newGetServerStrategiesResponse pHttpStatus_ =
  GetServerStrategiesResponse'
    { serverStrategies =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of strategy recommendations for the server.
getServerStrategiesResponse_serverStrategies :: Lens.Lens' GetServerStrategiesResponse (Prelude.Maybe [ServerStrategy])
getServerStrategiesResponse_serverStrategies = Lens.lens (\GetServerStrategiesResponse' {serverStrategies} -> serverStrategies) (\s@GetServerStrategiesResponse' {} a -> s {serverStrategies = a} :: GetServerStrategiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getServerStrategiesResponse_httpStatus :: Lens.Lens' GetServerStrategiesResponse Prelude.Int
getServerStrategiesResponse_httpStatus = Lens.lens (\GetServerStrategiesResponse' {httpStatus} -> httpStatus) (\s@GetServerStrategiesResponse' {} a -> s {httpStatus = a} :: GetServerStrategiesResponse)

instance Prelude.NFData GetServerStrategiesResponse where
  rnf GetServerStrategiesResponse' {..} =
    Prelude.rnf serverStrategies
      `Prelude.seq` Prelude.rnf httpStatus
