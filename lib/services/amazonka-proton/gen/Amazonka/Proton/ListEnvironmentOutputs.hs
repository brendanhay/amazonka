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
-- Module      : Amazonka.Proton.ListEnvironmentOutputs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the infrastructure as code outputs for your environment.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListEnvironmentOutputs
  ( -- * Creating a Request
    ListEnvironmentOutputs (..),
    newListEnvironmentOutputs,

    -- * Request Lenses
    listEnvironmentOutputs_nextToken,
    listEnvironmentOutputs_environmentName,

    -- * Destructuring the Response
    ListEnvironmentOutputsResponse (..),
    newListEnvironmentOutputsResponse,

    -- * Response Lenses
    listEnvironmentOutputsResponse_nextToken,
    listEnvironmentOutputsResponse_httpStatus,
    listEnvironmentOutputsResponse_outputs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEnvironmentOutputs' smart constructor.
data ListEnvironmentOutputs = ListEnvironmentOutputs'
  { -- | A token that indicates the location of the next environment output in
    -- the array of environment outputs, after the list of environment outputs
    -- that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The environment name.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentOutputs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentOutputs_nextToken' - A token that indicates the location of the next environment output in
-- the array of environment outputs, after the list of environment outputs
-- that was previously requested.
--
-- 'environmentName', 'listEnvironmentOutputs_environmentName' - The environment name.
newListEnvironmentOutputs ::
  -- | 'environmentName'
  Prelude.Text ->
  ListEnvironmentOutputs
newListEnvironmentOutputs pEnvironmentName_ =
  ListEnvironmentOutputs'
    { nextToken =
        Prelude.Nothing,
      environmentName = pEnvironmentName_
    }

-- | A token that indicates the location of the next environment output in
-- the array of environment outputs, after the list of environment outputs
-- that was previously requested.
listEnvironmentOutputs_nextToken :: Lens.Lens' ListEnvironmentOutputs (Prelude.Maybe Prelude.Text)
listEnvironmentOutputs_nextToken = Lens.lens (\ListEnvironmentOutputs' {nextToken} -> nextToken) (\s@ListEnvironmentOutputs' {} a -> s {nextToken = a} :: ListEnvironmentOutputs)

-- | The environment name.
listEnvironmentOutputs_environmentName :: Lens.Lens' ListEnvironmentOutputs Prelude.Text
listEnvironmentOutputs_environmentName = Lens.lens (\ListEnvironmentOutputs' {environmentName} -> environmentName) (\s@ListEnvironmentOutputs' {} a -> s {environmentName = a} :: ListEnvironmentOutputs)

instance Core.AWSPager ListEnvironmentOutputs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEnvironmentOutputsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listEnvironmentOutputsResponse_outputs) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEnvironmentOutputs_nextToken
          Lens..~ rs
          Lens.^? listEnvironmentOutputsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEnvironmentOutputs where
  type
    AWSResponse ListEnvironmentOutputs =
      ListEnvironmentOutputsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnvironmentOutputsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "outputs" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListEnvironmentOutputs where
  hashWithSalt _salt ListEnvironmentOutputs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData ListEnvironmentOutputs where
  rnf ListEnvironmentOutputs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders ListEnvironmentOutputs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListEnvironmentOutputs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEnvironmentOutputs where
  toJSON ListEnvironmentOutputs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("environmentName" Data..= environmentName)
          ]
      )

instance Data.ToPath ListEnvironmentOutputs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEnvironmentOutputs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEnvironmentOutputsResponse' smart constructor.
data ListEnvironmentOutputsResponse = ListEnvironmentOutputsResponse'
  { -- | A token that indicates the location of the next environment output in
    -- the array of environment outputs, after the current requested list of
    -- environment outputs.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of environment outputs with detail data.
    outputs :: [Data.Sensitive Output]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnvironmentOutputsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnvironmentOutputsResponse_nextToken' - A token that indicates the location of the next environment output in
-- the array of environment outputs, after the current requested list of
-- environment outputs.
--
-- 'httpStatus', 'listEnvironmentOutputsResponse_httpStatus' - The response's http status code.
--
-- 'outputs', 'listEnvironmentOutputsResponse_outputs' - An array of environment outputs with detail data.
newListEnvironmentOutputsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEnvironmentOutputsResponse
newListEnvironmentOutputsResponse pHttpStatus_ =
  ListEnvironmentOutputsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      outputs = Prelude.mempty
    }

-- | A token that indicates the location of the next environment output in
-- the array of environment outputs, after the current requested list of
-- environment outputs.
listEnvironmentOutputsResponse_nextToken :: Lens.Lens' ListEnvironmentOutputsResponse (Prelude.Maybe Prelude.Text)
listEnvironmentOutputsResponse_nextToken = Lens.lens (\ListEnvironmentOutputsResponse' {nextToken} -> nextToken) (\s@ListEnvironmentOutputsResponse' {} a -> s {nextToken = a} :: ListEnvironmentOutputsResponse)

-- | The response's http status code.
listEnvironmentOutputsResponse_httpStatus :: Lens.Lens' ListEnvironmentOutputsResponse Prelude.Int
listEnvironmentOutputsResponse_httpStatus = Lens.lens (\ListEnvironmentOutputsResponse' {httpStatus} -> httpStatus) (\s@ListEnvironmentOutputsResponse' {} a -> s {httpStatus = a} :: ListEnvironmentOutputsResponse)

-- | An array of environment outputs with detail data.
listEnvironmentOutputsResponse_outputs :: Lens.Lens' ListEnvironmentOutputsResponse [Output]
listEnvironmentOutputsResponse_outputs = Lens.lens (\ListEnvironmentOutputsResponse' {outputs} -> outputs) (\s@ListEnvironmentOutputsResponse' {} a -> s {outputs = a} :: ListEnvironmentOutputsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListEnvironmentOutputsResponse
  where
  rnf ListEnvironmentOutputsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf outputs
