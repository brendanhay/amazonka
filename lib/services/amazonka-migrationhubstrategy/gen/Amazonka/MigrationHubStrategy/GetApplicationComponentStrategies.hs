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
-- Module      : Amazonka.MigrationHubStrategy.GetApplicationComponentStrategies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all the recommended strategies and tools for an
-- application component running on a server.
module Amazonka.MigrationHubStrategy.GetApplicationComponentStrategies
  ( -- * Creating a Request
    GetApplicationComponentStrategies (..),
    newGetApplicationComponentStrategies,

    -- * Request Lenses
    getApplicationComponentStrategies_applicationComponentId,

    -- * Destructuring the Response
    GetApplicationComponentStrategiesResponse (..),
    newGetApplicationComponentStrategiesResponse,

    -- * Response Lenses
    getApplicationComponentStrategiesResponse_applicationComponentStrategies,
    getApplicationComponentStrategiesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetApplicationComponentStrategies' smart constructor.
data GetApplicationComponentStrategies = GetApplicationComponentStrategies'
  { -- | The ID of the application component. The ID is unique within an AWS
    -- account.
    applicationComponentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationComponentStrategies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationComponentId', 'getApplicationComponentStrategies_applicationComponentId' - The ID of the application component. The ID is unique within an AWS
-- account.
newGetApplicationComponentStrategies ::
  -- | 'applicationComponentId'
  Prelude.Text ->
  GetApplicationComponentStrategies
newGetApplicationComponentStrategies
  pApplicationComponentId_ =
    GetApplicationComponentStrategies'
      { applicationComponentId =
          pApplicationComponentId_
      }

-- | The ID of the application component. The ID is unique within an AWS
-- account.
getApplicationComponentStrategies_applicationComponentId :: Lens.Lens' GetApplicationComponentStrategies Prelude.Text
getApplicationComponentStrategies_applicationComponentId = Lens.lens (\GetApplicationComponentStrategies' {applicationComponentId} -> applicationComponentId) (\s@GetApplicationComponentStrategies' {} a -> s {applicationComponentId = a} :: GetApplicationComponentStrategies)

instance
  Core.AWSRequest
    GetApplicationComponentStrategies
  where
  type
    AWSResponse GetApplicationComponentStrategies =
      GetApplicationComponentStrategiesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationComponentStrategiesResponse'
            Prelude.<$> ( x
                            Data..?> "applicationComponentStrategies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetApplicationComponentStrategies
  where
  hashWithSalt
    _salt
    GetApplicationComponentStrategies' {..} =
      _salt `Prelude.hashWithSalt` applicationComponentId

instance
  Prelude.NFData
    GetApplicationComponentStrategies
  where
  rnf GetApplicationComponentStrategies' {..} =
    Prelude.rnf applicationComponentId

instance
  Data.ToHeaders
    GetApplicationComponentStrategies
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    GetApplicationComponentStrategies
  where
  toPath GetApplicationComponentStrategies' {..} =
    Prelude.mconcat
      [ "/get-applicationcomponent-strategies/",
        Data.toBS applicationComponentId
      ]

instance
  Data.ToQuery
    GetApplicationComponentStrategies
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApplicationComponentStrategiesResponse' smart constructor.
data GetApplicationComponentStrategiesResponse = GetApplicationComponentStrategiesResponse'
  { -- | A list of application component strategy recommendations.
    applicationComponentStrategies :: Prelude.Maybe [ApplicationComponentStrategy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationComponentStrategiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationComponentStrategies', 'getApplicationComponentStrategiesResponse_applicationComponentStrategies' - A list of application component strategy recommendations.
--
-- 'httpStatus', 'getApplicationComponentStrategiesResponse_httpStatus' - The response's http status code.
newGetApplicationComponentStrategiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApplicationComponentStrategiesResponse
newGetApplicationComponentStrategiesResponse
  pHttpStatus_ =
    GetApplicationComponentStrategiesResponse'
      { applicationComponentStrategies =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of application component strategy recommendations.
getApplicationComponentStrategiesResponse_applicationComponentStrategies :: Lens.Lens' GetApplicationComponentStrategiesResponse (Prelude.Maybe [ApplicationComponentStrategy])
getApplicationComponentStrategiesResponse_applicationComponentStrategies = Lens.lens (\GetApplicationComponentStrategiesResponse' {applicationComponentStrategies} -> applicationComponentStrategies) (\s@GetApplicationComponentStrategiesResponse' {} a -> s {applicationComponentStrategies = a} :: GetApplicationComponentStrategiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getApplicationComponentStrategiesResponse_httpStatus :: Lens.Lens' GetApplicationComponentStrategiesResponse Prelude.Int
getApplicationComponentStrategiesResponse_httpStatus = Lens.lens (\GetApplicationComponentStrategiesResponse' {httpStatus} -> httpStatus) (\s@GetApplicationComponentStrategiesResponse' {} a -> s {httpStatus = a} :: GetApplicationComponentStrategiesResponse)

instance
  Prelude.NFData
    GetApplicationComponentStrategiesResponse
  where
  rnf GetApplicationComponentStrategiesResponse' {..} =
    Prelude.rnf applicationComponentStrategies `Prelude.seq`
      Prelude.rnf httpStatus
