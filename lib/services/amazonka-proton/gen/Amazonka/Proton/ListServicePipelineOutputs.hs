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
-- Module      : Amazonka.Proton.ListServicePipelineOutputs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of service pipeline Infrastructure as Code (IaC) outputs.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListServicePipelineOutputs
  ( -- * Creating a Request
    ListServicePipelineOutputs (..),
    newListServicePipelineOutputs,

    -- * Request Lenses
    listServicePipelineOutputs_nextToken,
    listServicePipelineOutputs_serviceName,

    -- * Destructuring the Response
    ListServicePipelineOutputsResponse (..),
    newListServicePipelineOutputsResponse,

    -- * Response Lenses
    listServicePipelineOutputsResponse_nextToken,
    listServicePipelineOutputsResponse_httpStatus,
    listServicePipelineOutputsResponse_outputs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServicePipelineOutputs' smart constructor.
data ListServicePipelineOutputs = ListServicePipelineOutputs'
  { -- | A token that indicates the location of the next output in the array of
    -- outputs, after the list of outputs that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the service whose pipeline\'s outputs you want.
    serviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServicePipelineOutputs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServicePipelineOutputs_nextToken' - A token that indicates the location of the next output in the array of
-- outputs, after the list of outputs that was previously requested.
--
-- 'serviceName', 'listServicePipelineOutputs_serviceName' - The name of the service whose pipeline\'s outputs you want.
newListServicePipelineOutputs ::
  -- | 'serviceName'
  Prelude.Text ->
  ListServicePipelineOutputs
newListServicePipelineOutputs pServiceName_ =
  ListServicePipelineOutputs'
    { nextToken =
        Prelude.Nothing,
      serviceName = pServiceName_
    }

-- | A token that indicates the location of the next output in the array of
-- outputs, after the list of outputs that was previously requested.
listServicePipelineOutputs_nextToken :: Lens.Lens' ListServicePipelineOutputs (Prelude.Maybe Prelude.Text)
listServicePipelineOutputs_nextToken = Lens.lens (\ListServicePipelineOutputs' {nextToken} -> nextToken) (\s@ListServicePipelineOutputs' {} a -> s {nextToken = a} :: ListServicePipelineOutputs)

-- | The name of the service whose pipeline\'s outputs you want.
listServicePipelineOutputs_serviceName :: Lens.Lens' ListServicePipelineOutputs Prelude.Text
listServicePipelineOutputs_serviceName = Lens.lens (\ListServicePipelineOutputs' {serviceName} -> serviceName) (\s@ListServicePipelineOutputs' {} a -> s {serviceName = a} :: ListServicePipelineOutputs)

instance Core.AWSPager ListServicePipelineOutputs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServicePipelineOutputsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listServicePipelineOutputsResponse_outputs
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listServicePipelineOutputs_nextToken
          Lens..~ rs
          Lens.^? listServicePipelineOutputsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListServicePipelineOutputs where
  type
    AWSResponse ListServicePipelineOutputs =
      ListServicePipelineOutputsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServicePipelineOutputsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "outputs" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListServicePipelineOutputs where
  hashWithSalt _salt ListServicePipelineOutputs' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceName

instance Prelude.NFData ListServicePipelineOutputs where
  rnf ListServicePipelineOutputs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceName

instance Data.ToHeaders ListServicePipelineOutputs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListServicePipelineOutputs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListServicePipelineOutputs where
  toJSON ListServicePipelineOutputs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("serviceName" Data..= serviceName)
          ]
      )

instance Data.ToPath ListServicePipelineOutputs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListServicePipelineOutputs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServicePipelineOutputsResponse' smart constructor.
data ListServicePipelineOutputsResponse = ListServicePipelineOutputsResponse'
  { -- | A token that indicates the location of the next output in the array of
    -- outputs, after the current requested list of outputs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of service pipeline Infrastructure as Code (IaC) outputs.
    outputs :: [Data.Sensitive Output]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServicePipelineOutputsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServicePipelineOutputsResponse_nextToken' - A token that indicates the location of the next output in the array of
-- outputs, after the current requested list of outputs.
--
-- 'httpStatus', 'listServicePipelineOutputsResponse_httpStatus' - The response's http status code.
--
-- 'outputs', 'listServicePipelineOutputsResponse_outputs' - An array of service pipeline Infrastructure as Code (IaC) outputs.
newListServicePipelineOutputsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServicePipelineOutputsResponse
newListServicePipelineOutputsResponse pHttpStatus_ =
  ListServicePipelineOutputsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      outputs = Prelude.mempty
    }

-- | A token that indicates the location of the next output in the array of
-- outputs, after the current requested list of outputs.
listServicePipelineOutputsResponse_nextToken :: Lens.Lens' ListServicePipelineOutputsResponse (Prelude.Maybe Prelude.Text)
listServicePipelineOutputsResponse_nextToken = Lens.lens (\ListServicePipelineOutputsResponse' {nextToken} -> nextToken) (\s@ListServicePipelineOutputsResponse' {} a -> s {nextToken = a} :: ListServicePipelineOutputsResponse)

-- | The response's http status code.
listServicePipelineOutputsResponse_httpStatus :: Lens.Lens' ListServicePipelineOutputsResponse Prelude.Int
listServicePipelineOutputsResponse_httpStatus = Lens.lens (\ListServicePipelineOutputsResponse' {httpStatus} -> httpStatus) (\s@ListServicePipelineOutputsResponse' {} a -> s {httpStatus = a} :: ListServicePipelineOutputsResponse)

-- | An array of service pipeline Infrastructure as Code (IaC) outputs.
listServicePipelineOutputsResponse_outputs :: Lens.Lens' ListServicePipelineOutputsResponse [Output]
listServicePipelineOutputsResponse_outputs = Lens.lens (\ListServicePipelineOutputsResponse' {outputs} -> outputs) (\s@ListServicePipelineOutputsResponse' {} a -> s {outputs = a} :: ListServicePipelineOutputsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListServicePipelineOutputsResponse
  where
  rnf ListServicePipelineOutputsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf outputs
