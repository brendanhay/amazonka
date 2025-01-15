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
-- Module      : Amazonka.Proton.ListServiceInstanceOutputs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list service of instance Infrastructure as Code (IaC) outputs.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListServiceInstanceOutputs
  ( -- * Creating a Request
    ListServiceInstanceOutputs (..),
    newListServiceInstanceOutputs,

    -- * Request Lenses
    listServiceInstanceOutputs_nextToken,
    listServiceInstanceOutputs_serviceInstanceName,
    listServiceInstanceOutputs_serviceName,

    -- * Destructuring the Response
    ListServiceInstanceOutputsResponse (..),
    newListServiceInstanceOutputsResponse,

    -- * Response Lenses
    listServiceInstanceOutputsResponse_nextToken,
    listServiceInstanceOutputsResponse_httpStatus,
    listServiceInstanceOutputsResponse_outputs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServiceInstanceOutputs' smart constructor.
data ListServiceInstanceOutputs = ListServiceInstanceOutputs'
  { -- | A token that indicates the location of the next output in the array of
    -- outputs, after the list of outputs that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the service instance whose outputs you want.
    serviceInstanceName :: Prelude.Text,
    -- | The name of the service that @serviceInstanceName@ is associated to.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceInstanceOutputs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceInstanceOutputs_nextToken' - A token that indicates the location of the next output in the array of
-- outputs, after the list of outputs that was previously requested.
--
-- 'serviceInstanceName', 'listServiceInstanceOutputs_serviceInstanceName' - The name of the service instance whose outputs you want.
--
-- 'serviceName', 'listServiceInstanceOutputs_serviceName' - The name of the service that @serviceInstanceName@ is associated to.
newListServiceInstanceOutputs ::
  -- | 'serviceInstanceName'
  Prelude.Text ->
  -- | 'serviceName'
  Prelude.Text ->
  ListServiceInstanceOutputs
newListServiceInstanceOutputs
  pServiceInstanceName_
  pServiceName_ =
    ListServiceInstanceOutputs'
      { nextToken =
          Prelude.Nothing,
        serviceInstanceName = pServiceInstanceName_,
        serviceName = pServiceName_
      }

-- | A token that indicates the location of the next output in the array of
-- outputs, after the list of outputs that was previously requested.
listServiceInstanceOutputs_nextToken :: Lens.Lens' ListServiceInstanceOutputs (Prelude.Maybe Prelude.Text)
listServiceInstanceOutputs_nextToken = Lens.lens (\ListServiceInstanceOutputs' {nextToken} -> nextToken) (\s@ListServiceInstanceOutputs' {} a -> s {nextToken = a} :: ListServiceInstanceOutputs)

-- | The name of the service instance whose outputs you want.
listServiceInstanceOutputs_serviceInstanceName :: Lens.Lens' ListServiceInstanceOutputs Prelude.Text
listServiceInstanceOutputs_serviceInstanceName = Lens.lens (\ListServiceInstanceOutputs' {serviceInstanceName} -> serviceInstanceName) (\s@ListServiceInstanceOutputs' {} a -> s {serviceInstanceName = a} :: ListServiceInstanceOutputs)

-- | The name of the service that @serviceInstanceName@ is associated to.
listServiceInstanceOutputs_serviceName :: Lens.Lens' ListServiceInstanceOutputs Prelude.Text
listServiceInstanceOutputs_serviceName = Lens.lens (\ListServiceInstanceOutputs' {serviceName} -> serviceName) (\s@ListServiceInstanceOutputs' {} a -> s {serviceName = a} :: ListServiceInstanceOutputs)

instance Core.AWSPager ListServiceInstanceOutputs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServiceInstanceOutputsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listServiceInstanceOutputsResponse_outputs
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listServiceInstanceOutputs_nextToken
              Lens..~ rs
              Lens.^? listServiceInstanceOutputsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListServiceInstanceOutputs where
  type
    AWSResponse ListServiceInstanceOutputs =
      ListServiceInstanceOutputsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceInstanceOutputsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "outputs" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListServiceInstanceOutputs where
  hashWithSalt _salt ListServiceInstanceOutputs' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceInstanceName
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ListServiceInstanceOutputs where
  rnf ListServiceInstanceOutputs' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf serviceInstanceName `Prelude.seq`
        Prelude.rnf serviceName

instance Data.ToHeaders ListServiceInstanceOutputs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListServiceInstanceOutputs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListServiceInstanceOutputs where
  toJSON ListServiceInstanceOutputs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("serviceInstanceName" Data..= serviceInstanceName),
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance Data.ToPath ListServiceInstanceOutputs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListServiceInstanceOutputs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServiceInstanceOutputsResponse' smart constructor.
data ListServiceInstanceOutputsResponse = ListServiceInstanceOutputsResponse'
  { -- | A token that indicates the location of the next output in the array of
    -- outputs, after the current requested list of outputs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of service instance Infrastructure as Code (IaC) outputs.
    outputs :: [Data.Sensitive Output]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceInstanceOutputsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceInstanceOutputsResponse_nextToken' - A token that indicates the location of the next output in the array of
-- outputs, after the current requested list of outputs.
--
-- 'httpStatus', 'listServiceInstanceOutputsResponse_httpStatus' - The response's http status code.
--
-- 'outputs', 'listServiceInstanceOutputsResponse_outputs' - An array of service instance Infrastructure as Code (IaC) outputs.
newListServiceInstanceOutputsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceInstanceOutputsResponse
newListServiceInstanceOutputsResponse pHttpStatus_ =
  ListServiceInstanceOutputsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      outputs = Prelude.mempty
    }

-- | A token that indicates the location of the next output in the array of
-- outputs, after the current requested list of outputs.
listServiceInstanceOutputsResponse_nextToken :: Lens.Lens' ListServiceInstanceOutputsResponse (Prelude.Maybe Prelude.Text)
listServiceInstanceOutputsResponse_nextToken = Lens.lens (\ListServiceInstanceOutputsResponse' {nextToken} -> nextToken) (\s@ListServiceInstanceOutputsResponse' {} a -> s {nextToken = a} :: ListServiceInstanceOutputsResponse)

-- | The response's http status code.
listServiceInstanceOutputsResponse_httpStatus :: Lens.Lens' ListServiceInstanceOutputsResponse Prelude.Int
listServiceInstanceOutputsResponse_httpStatus = Lens.lens (\ListServiceInstanceOutputsResponse' {httpStatus} -> httpStatus) (\s@ListServiceInstanceOutputsResponse' {} a -> s {httpStatus = a} :: ListServiceInstanceOutputsResponse)

-- | An array of service instance Infrastructure as Code (IaC) outputs.
listServiceInstanceOutputsResponse_outputs :: Lens.Lens' ListServiceInstanceOutputsResponse [Output]
listServiceInstanceOutputsResponse_outputs = Lens.lens (\ListServiceInstanceOutputsResponse' {outputs} -> outputs) (\s@ListServiceInstanceOutputsResponse' {} a -> s {outputs = a} :: ListServiceInstanceOutputsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListServiceInstanceOutputsResponse
  where
  rnf ListServiceInstanceOutputsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf outputs
