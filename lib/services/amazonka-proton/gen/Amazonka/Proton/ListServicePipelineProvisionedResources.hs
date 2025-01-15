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
-- Module      : Amazonka.Proton.ListServicePipelineProvisionedResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List provisioned resources for a service and pipeline with details.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListServicePipelineProvisionedResources
  ( -- * Creating a Request
    ListServicePipelineProvisionedResources (..),
    newListServicePipelineProvisionedResources,

    -- * Request Lenses
    listServicePipelineProvisionedResources_nextToken,
    listServicePipelineProvisionedResources_serviceName,

    -- * Destructuring the Response
    ListServicePipelineProvisionedResourcesResponse (..),
    newListServicePipelineProvisionedResourcesResponse,

    -- * Response Lenses
    listServicePipelineProvisionedResourcesResponse_nextToken,
    listServicePipelineProvisionedResourcesResponse_httpStatus,
    listServicePipelineProvisionedResourcesResponse_provisionedResources,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServicePipelineProvisionedResources' smart constructor.
data ListServicePipelineProvisionedResources = ListServicePipelineProvisionedResources'
  { -- | A token that indicates the location of the next provisioned resource in
    -- the array of provisioned resources, after the list of provisioned
    -- resources that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the service whose pipeline\'s provisioned resources you
    -- want.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServicePipelineProvisionedResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServicePipelineProvisionedResources_nextToken' - A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the list of provisioned
-- resources that was previously requested.
--
-- 'serviceName', 'listServicePipelineProvisionedResources_serviceName' - The name of the service whose pipeline\'s provisioned resources you
-- want.
newListServicePipelineProvisionedResources ::
  -- | 'serviceName'
  Prelude.Text ->
  ListServicePipelineProvisionedResources
newListServicePipelineProvisionedResources
  pServiceName_ =
    ListServicePipelineProvisionedResources'
      { nextToken =
          Prelude.Nothing,
        serviceName = pServiceName_
      }

-- | A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the list of provisioned
-- resources that was previously requested.
listServicePipelineProvisionedResources_nextToken :: Lens.Lens' ListServicePipelineProvisionedResources (Prelude.Maybe Prelude.Text)
listServicePipelineProvisionedResources_nextToken = Lens.lens (\ListServicePipelineProvisionedResources' {nextToken} -> nextToken) (\s@ListServicePipelineProvisionedResources' {} a -> s {nextToken = a} :: ListServicePipelineProvisionedResources)

-- | The name of the service whose pipeline\'s provisioned resources you
-- want.
listServicePipelineProvisionedResources_serviceName :: Lens.Lens' ListServicePipelineProvisionedResources Prelude.Text
listServicePipelineProvisionedResources_serviceName = Lens.lens (\ListServicePipelineProvisionedResources' {serviceName} -> serviceName) (\s@ListServicePipelineProvisionedResources' {} a -> s {serviceName = a} :: ListServicePipelineProvisionedResources)

instance
  Core.AWSPager
    ListServicePipelineProvisionedResources
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServicePipelineProvisionedResourcesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listServicePipelineProvisionedResourcesResponse_provisionedResources
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listServicePipelineProvisionedResources_nextToken
              Lens..~ rs
              Lens.^? listServicePipelineProvisionedResourcesResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListServicePipelineProvisionedResources
  where
  type
    AWSResponse
      ListServicePipelineProvisionedResources =
      ListServicePipelineProvisionedResourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServicePipelineProvisionedResourcesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "provisionedResources"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListServicePipelineProvisionedResources
  where
  hashWithSalt
    _salt
    ListServicePipelineProvisionedResources' {..} =
      _salt
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` serviceName

instance
  Prelude.NFData
    ListServicePipelineProvisionedResources
  where
  rnf ListServicePipelineProvisionedResources' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf serviceName

instance
  Data.ToHeaders
    ListServicePipelineProvisionedResources
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListServicePipelineProvisionedResources" ::
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
    ListServicePipelineProvisionedResources
  where
  toJSON ListServicePipelineProvisionedResources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance
  Data.ToPath
    ListServicePipelineProvisionedResources
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListServicePipelineProvisionedResources
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServicePipelineProvisionedResourcesResponse' smart constructor.
data ListServicePipelineProvisionedResourcesResponse = ListServicePipelineProvisionedResourcesResponse'
  { -- | A token that indicates the location of the next provisioned resource in
    -- the array of provisioned resources, after the current requested list of
    -- provisioned resources.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of provisioned resources for a service and pipeline.
    provisionedResources :: [ProvisionedResource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServicePipelineProvisionedResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServicePipelineProvisionedResourcesResponse_nextToken' - A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the current requested list of
-- provisioned resources.
--
-- 'httpStatus', 'listServicePipelineProvisionedResourcesResponse_httpStatus' - The response's http status code.
--
-- 'provisionedResources', 'listServicePipelineProvisionedResourcesResponse_provisionedResources' - An array of provisioned resources for a service and pipeline.
newListServicePipelineProvisionedResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServicePipelineProvisionedResourcesResponse
newListServicePipelineProvisionedResourcesResponse
  pHttpStatus_ =
    ListServicePipelineProvisionedResourcesResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        provisionedResources =
          Prelude.mempty
      }

-- | A token that indicates the location of the next provisioned resource in
-- the array of provisioned resources, after the current requested list of
-- provisioned resources.
listServicePipelineProvisionedResourcesResponse_nextToken :: Lens.Lens' ListServicePipelineProvisionedResourcesResponse (Prelude.Maybe Prelude.Text)
listServicePipelineProvisionedResourcesResponse_nextToken = Lens.lens (\ListServicePipelineProvisionedResourcesResponse' {nextToken} -> nextToken) (\s@ListServicePipelineProvisionedResourcesResponse' {} a -> s {nextToken = a} :: ListServicePipelineProvisionedResourcesResponse)

-- | The response's http status code.
listServicePipelineProvisionedResourcesResponse_httpStatus :: Lens.Lens' ListServicePipelineProvisionedResourcesResponse Prelude.Int
listServicePipelineProvisionedResourcesResponse_httpStatus = Lens.lens (\ListServicePipelineProvisionedResourcesResponse' {httpStatus} -> httpStatus) (\s@ListServicePipelineProvisionedResourcesResponse' {} a -> s {httpStatus = a} :: ListServicePipelineProvisionedResourcesResponse)

-- | An array of provisioned resources for a service and pipeline.
listServicePipelineProvisionedResourcesResponse_provisionedResources :: Lens.Lens' ListServicePipelineProvisionedResourcesResponse [ProvisionedResource]
listServicePipelineProvisionedResourcesResponse_provisionedResources = Lens.lens (\ListServicePipelineProvisionedResourcesResponse' {provisionedResources} -> provisionedResources) (\s@ListServicePipelineProvisionedResourcesResponse' {} a -> s {provisionedResources = a} :: ListServicePipelineProvisionedResourcesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListServicePipelineProvisionedResourcesResponse
  where
  rnf
    ListServicePipelineProvisionedResourcesResponse' {..} =
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf provisionedResources
