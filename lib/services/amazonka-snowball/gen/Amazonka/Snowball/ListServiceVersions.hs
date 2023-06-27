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
-- Module      : Amazonka.Snowball.ListServiceVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all supported versions for Snow on-device services. Returns an
-- array of @ServiceVersion@ object containing the supported versions for a
-- particular service.
module Amazonka.Snowball.ListServiceVersions
  ( -- * Creating a Request
    ListServiceVersions (..),
    newListServiceVersions,

    -- * Request Lenses
    listServiceVersions_dependentServices,
    listServiceVersions_maxResults,
    listServiceVersions_nextToken,
    listServiceVersions_serviceName,

    -- * Destructuring the Response
    ListServiceVersionsResponse (..),
    newListServiceVersionsResponse,

    -- * Response Lenses
    listServiceVersionsResponse_dependentServices,
    listServiceVersionsResponse_nextToken,
    listServiceVersionsResponse_httpStatus,
    listServiceVersionsResponse_serviceVersions,
    listServiceVersionsResponse_serviceName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newListServiceVersions' smart constructor.
data ListServiceVersions = ListServiceVersions'
  { -- | A list of names and versions of dependant services of the requested
    -- service.
    dependentServices :: Prelude.Maybe [DependentService],
    -- | The maximum number of @ListServiceVersions@ objects to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Because HTTP requests are stateless, this is the starting point for the
    -- next list of returned @ListServiceVersionsRequest@ versions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the service for which you\'re requesting supported versions.
    serviceName :: ServiceName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dependentServices', 'listServiceVersions_dependentServices' - A list of names and versions of dependant services of the requested
-- service.
--
-- 'maxResults', 'listServiceVersions_maxResults' - The maximum number of @ListServiceVersions@ objects to return.
--
-- 'nextToken', 'listServiceVersions_nextToken' - Because HTTP requests are stateless, this is the starting point for the
-- next list of returned @ListServiceVersionsRequest@ versions.
--
-- 'serviceName', 'listServiceVersions_serviceName' - The name of the service for which you\'re requesting supported versions.
newListServiceVersions ::
  -- | 'serviceName'
  ServiceName ->
  ListServiceVersions
newListServiceVersions pServiceName_ =
  ListServiceVersions'
    { dependentServices =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceName = pServiceName_
    }

-- | A list of names and versions of dependant services of the requested
-- service.
listServiceVersions_dependentServices :: Lens.Lens' ListServiceVersions (Prelude.Maybe [DependentService])
listServiceVersions_dependentServices = Lens.lens (\ListServiceVersions' {dependentServices} -> dependentServices) (\s@ListServiceVersions' {} a -> s {dependentServices = a} :: ListServiceVersions) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of @ListServiceVersions@ objects to return.
listServiceVersions_maxResults :: Lens.Lens' ListServiceVersions (Prelude.Maybe Prelude.Natural)
listServiceVersions_maxResults = Lens.lens (\ListServiceVersions' {maxResults} -> maxResults) (\s@ListServiceVersions' {} a -> s {maxResults = a} :: ListServiceVersions)

-- | Because HTTP requests are stateless, this is the starting point for the
-- next list of returned @ListServiceVersionsRequest@ versions.
listServiceVersions_nextToken :: Lens.Lens' ListServiceVersions (Prelude.Maybe Prelude.Text)
listServiceVersions_nextToken = Lens.lens (\ListServiceVersions' {nextToken} -> nextToken) (\s@ListServiceVersions' {} a -> s {nextToken = a} :: ListServiceVersions)

-- | The name of the service for which you\'re requesting supported versions.
listServiceVersions_serviceName :: Lens.Lens' ListServiceVersions ServiceName
listServiceVersions_serviceName = Lens.lens (\ListServiceVersions' {serviceName} -> serviceName) (\s@ListServiceVersions' {} a -> s {serviceName = a} :: ListServiceVersions)

instance Core.AWSRequest ListServiceVersions where
  type
    AWSResponse ListServiceVersions =
      ListServiceVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceVersionsResponse'
            Prelude.<$> ( x
                            Data..?> "DependentServices"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "ServiceVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "ServiceName")
      )

instance Prelude.Hashable ListServiceVersions where
  hashWithSalt _salt ListServiceVersions' {..} =
    _salt
      `Prelude.hashWithSalt` dependentServices
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ListServiceVersions where
  rnf ListServiceVersions' {..} =
    Prelude.rnf dependentServices
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders ListServiceVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.ListServiceVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListServiceVersions where
  toJSON ListServiceVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DependentServices" Data..=)
              Prelude.<$> dependentServices,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ServiceName" Data..= serviceName)
          ]
      )

instance Data.ToPath ListServiceVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListServiceVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServiceVersionsResponse' smart constructor.
data ListServiceVersionsResponse = ListServiceVersionsResponse'
  { -- | A list of names and versions of dependant services of the service for
    -- which the system provided supported versions.
    dependentServices :: Prelude.Maybe [DependentService],
    -- | Because HTTP requests are stateless, this is the starting point of the
    -- next list of returned @ListServiceVersionsResult@ results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of supported versions.
    serviceVersions :: [ServiceVersion],
    -- | The name of the service for which the system provided supported
    -- versions.
    serviceName :: ServiceName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dependentServices', 'listServiceVersionsResponse_dependentServices' - A list of names and versions of dependant services of the service for
-- which the system provided supported versions.
--
-- 'nextToken', 'listServiceVersionsResponse_nextToken' - Because HTTP requests are stateless, this is the starting point of the
-- next list of returned @ListServiceVersionsResult@ results.
--
-- 'httpStatus', 'listServiceVersionsResponse_httpStatus' - The response's http status code.
--
-- 'serviceVersions', 'listServiceVersionsResponse_serviceVersions' - A list of supported versions.
--
-- 'serviceName', 'listServiceVersionsResponse_serviceName' - The name of the service for which the system provided supported
-- versions.
newListServiceVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceName'
  ServiceName ->
  ListServiceVersionsResponse
newListServiceVersionsResponse
  pHttpStatus_
  pServiceName_ =
    ListServiceVersionsResponse'
      { dependentServices =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        serviceVersions = Prelude.mempty,
        serviceName = pServiceName_
      }

-- | A list of names and versions of dependant services of the service for
-- which the system provided supported versions.
listServiceVersionsResponse_dependentServices :: Lens.Lens' ListServiceVersionsResponse (Prelude.Maybe [DependentService])
listServiceVersionsResponse_dependentServices = Lens.lens (\ListServiceVersionsResponse' {dependentServices} -> dependentServices) (\s@ListServiceVersionsResponse' {} a -> s {dependentServices = a} :: ListServiceVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Because HTTP requests are stateless, this is the starting point of the
-- next list of returned @ListServiceVersionsResult@ results.
listServiceVersionsResponse_nextToken :: Lens.Lens' ListServiceVersionsResponse (Prelude.Maybe Prelude.Text)
listServiceVersionsResponse_nextToken = Lens.lens (\ListServiceVersionsResponse' {nextToken} -> nextToken) (\s@ListServiceVersionsResponse' {} a -> s {nextToken = a} :: ListServiceVersionsResponse)

-- | The response's http status code.
listServiceVersionsResponse_httpStatus :: Lens.Lens' ListServiceVersionsResponse Prelude.Int
listServiceVersionsResponse_httpStatus = Lens.lens (\ListServiceVersionsResponse' {httpStatus} -> httpStatus) (\s@ListServiceVersionsResponse' {} a -> s {httpStatus = a} :: ListServiceVersionsResponse)

-- | A list of supported versions.
listServiceVersionsResponse_serviceVersions :: Lens.Lens' ListServiceVersionsResponse [ServiceVersion]
listServiceVersionsResponse_serviceVersions = Lens.lens (\ListServiceVersionsResponse' {serviceVersions} -> serviceVersions) (\s@ListServiceVersionsResponse' {} a -> s {serviceVersions = a} :: ListServiceVersionsResponse) Prelude.. Lens.coerced

-- | The name of the service for which the system provided supported
-- versions.
listServiceVersionsResponse_serviceName :: Lens.Lens' ListServiceVersionsResponse ServiceName
listServiceVersionsResponse_serviceName = Lens.lens (\ListServiceVersionsResponse' {serviceName} -> serviceName) (\s@ListServiceVersionsResponse' {} a -> s {serviceName = a} :: ListServiceVersionsResponse)

instance Prelude.NFData ListServiceVersionsResponse where
  rnf ListServiceVersionsResponse' {..} =
    Prelude.rnf dependentServices
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceVersions
      `Prelude.seq` Prelude.rnf serviceName
