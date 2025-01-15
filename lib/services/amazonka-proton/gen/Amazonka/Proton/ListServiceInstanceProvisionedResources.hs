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
-- Module      : Amazonka.Proton.ListServiceInstanceProvisionedResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List provisioned resources for a service instance with details.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListServiceInstanceProvisionedResources
  ( -- * Creating a Request
    ListServiceInstanceProvisionedResources (..),
    newListServiceInstanceProvisionedResources,

    -- * Request Lenses
    listServiceInstanceProvisionedResources_nextToken,
    listServiceInstanceProvisionedResources_serviceInstanceName,
    listServiceInstanceProvisionedResources_serviceName,

    -- * Destructuring the Response
    ListServiceInstanceProvisionedResourcesResponse (..),
    newListServiceInstanceProvisionedResourcesResponse,

    -- * Response Lenses
    listServiceInstanceProvisionedResourcesResponse_nextToken,
    listServiceInstanceProvisionedResourcesResponse_httpStatus,
    listServiceInstanceProvisionedResourcesResponse_provisionedResources,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServiceInstanceProvisionedResources' smart constructor.
data ListServiceInstanceProvisionedResources = ListServiceInstanceProvisionedResources'
  { -- | A token that indicates the location of the next provisioned resource in
    -- the array of provisioned resources, after the list of provisioned
    -- resources that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the service instance whose provisioned resources you want.
    serviceInstanceName :: Prelude.Text,
    -- | The name of the service that @serviceInstanceName@ is associated to.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceInstanceProvisionedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceInstanceProvisionedResources_nextToken' - A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the list of provisioned
-- resources that was previously requested.
--
-- 'serviceInstanceName', 'listServiceInstanceProvisionedResources_serviceInstanceName' - The name of the service instance whose provisioned resources you want.
--
-- 'serviceName', 'listServiceInstanceProvisionedResources_serviceName' - The name of the service that @serviceInstanceName@ is associated to.
newListServiceInstanceProvisionedResources ::
  -- | 'serviceInstanceName'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  ListServiceInstanceProvisionedResources
newListServiceInstanceProvisionedResources
  pServiceInstanceName_
  pServiceName_ =
    ListServiceInstanceProvisionedResources'
      { nextToken =
          Prelude.Nothing,
        serviceInstanceName =
          pServiceInstanceName_,
        serviceName = pServiceName_
      }

-- | A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the list of provisioned
-- resources that was previously requested.
listServiceInstanceProvisionedResources_nextToken :: Lens.Lens' ListServiceInstanceProvisionedResources (Prelude.Maybe Prelude.Text)
listServiceInstanceProvisionedResources_nextToken = Lens.lens (\ListServiceInstanceProvisionedResources' {nextToken} -> nextToken) (\s@ListServiceInstanceProvisionedResources' {} a -> s {nextToken = a} :: ListServiceInstanceProvisionedResources)

-- | The name of the service instance whose provisioned resources you want.
listServiceInstanceProvisionedResources_serviceInstanceName :: Lens.Lens' ListServiceInstanceProvisionedResources Prelude.Text
listServiceInstanceProvisionedResources_serviceInstanceName = Lens.lens (\ListServiceInstanceProvisionedResources' {serviceInstanceName} -> serviceInstanceName) (\s@ListServiceInstanceProvisionedResources' {} a -> s {serviceInstanceName = a} :: ListServiceInstanceProvisionedResources)

-- | The name of the service that @serviceInstanceName@ is associated to.
listServiceInstanceProvisionedResources_serviceName :: Lens.Lens' ListServiceInstanceProvisionedResources Prelude.Text
listServiceInstanceProvisionedResources_serviceName = Lens.lens (\ListServiceInstanceProvisionedResources' {serviceName} -> serviceName) (\s@ListServiceInstanceProvisionedResources' {} a -> s {serviceName = a} :: ListServiceInstanceProvisionedResources)

instance
  Core.AWSPager
    ListServiceInstanceProvisionedResources
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServiceInstanceProvisionedResourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listServiceInstanceProvisionedResourcesResponse_provisionedResources
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listServiceInstanceProvisionedResources_nextToken
              Lens..~ rs
              Lens.^? listServiceInstanceProvisionedResourcesResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListServiceInstanceProvisionedResources
  where
  type
    AWSResponse
      ListServiceInstanceProvisionedResources =
      ListServiceInstanceProvisionedResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceInstanceProvisionedResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "provisionedResources"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListServiceInstanceProvisionedResources
  where
  hashWithSalt
    _salt
    ListServiceInstanceProvisionedResources' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` serviceInstanceName
        `Prelude.hashWithSalt` serviceName

instance
  Prelude.NFData
    ListServiceInstanceProvisionedResources
  where
  rnf ListServiceInstanceProvisionedResources' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf serviceInstanceName `Prelude.seq`
        Prelude.rnf serviceName

instance
  Data.ToHeaders
    ListServiceInstanceProvisionedResources
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListServiceInstanceProvisionedResources" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListServiceInstanceProvisionedResources
  where
  toJSON ListServiceInstanceProvisionedResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("serviceInstanceName" Data..= serviceInstanceName),
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance
  Data.ToPath
    ListServiceInstanceProvisionedResources
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListServiceInstanceProvisionedResources
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServiceInstanceProvisionedResourcesResponse' smart constructor.
data ListServiceInstanceProvisionedResourcesResponse = ListServiceInstanceProvisionedResourcesResponse'
  { -- | A token that indicates the location of the next provisioned resource in
    -- the array of provisioned resources, after the current requested list of
    -- provisioned resources.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of provisioned resources for a service instance.
    provisionedResources :: [ProvisionedResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceInstanceProvisionedResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceInstanceProvisionedResourcesResponse_nextToken' - A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the current requested list of
-- provisioned resources.
--
-- 'httpStatus', 'listServiceInstanceProvisionedResourcesResponse_httpStatus' - The response's http status code.
--
-- 'provisionedResources', 'listServiceInstanceProvisionedResourcesResponse_provisionedResources' - An array of provisioned resources for a service instance.
newListServiceInstanceProvisionedResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceInstanceProvisionedResourcesResponse
newListServiceInstanceProvisionedResourcesResponse
  pHttpStatus_ =
    ListServiceInstanceProvisionedResourcesResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        provisionedResources =
          Prelude.mempty
      }

-- | A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the current requested list of
-- provisioned resources.
listServiceInstanceProvisionedResourcesResponse_nextToken :: Lens.Lens' ListServiceInstanceProvisionedResourcesResponse (Prelude.Maybe Prelude.Text)
listServiceInstanceProvisionedResourcesResponse_nextToken = Lens.lens (\ListServiceInstanceProvisionedResourcesResponse' {nextToken} -> nextToken) (\s@ListServiceInstanceProvisionedResourcesResponse' {} a -> s {nextToken = a} :: ListServiceInstanceProvisionedResourcesResponse)

-- | The response's http status code.
listServiceInstanceProvisionedResourcesResponse_httpStatus :: Lens.Lens' ListServiceInstanceProvisionedResourcesResponse Prelude.Int
listServiceInstanceProvisionedResourcesResponse_httpStatus = Lens.lens (\ListServiceInstanceProvisionedResourcesResponse' {httpStatus} -> httpStatus) (\s@ListServiceInstanceProvisionedResourcesResponse' {} a -> s {httpStatus = a} :: ListServiceInstanceProvisionedResourcesResponse)

-- | An array of provisioned resources for a service instance.
listServiceInstanceProvisionedResourcesResponse_provisionedResources :: Lens.Lens' ListServiceInstanceProvisionedResourcesResponse [ProvisionedResource]
listServiceInstanceProvisionedResourcesResponse_provisionedResources = Lens.lens (\ListServiceInstanceProvisionedResourcesResponse' {provisionedResources} -> provisionedResources) (\s@ListServiceInstanceProvisionedResourcesResponse' {} a -> s {provisionedResources = a} :: ListServiceInstanceProvisionedResourcesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListServiceInstanceProvisionedResourcesResponse
  where
  rnf
    ListServiceInstanceProvisionedResourcesResponse' {..} =
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf provisionedResources
