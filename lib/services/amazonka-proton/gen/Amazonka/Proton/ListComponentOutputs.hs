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
-- Module      : Amazonka.Proton.ListComponentOutputs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a list of component Infrastructure as Code (IaC) outputs.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListComponentOutputs
  ( -- * Creating a Request
    ListComponentOutputs (..),
    newListComponentOutputs,

    -- * Request Lenses
    listComponentOutputs_nextToken,
    listComponentOutputs_componentName,

    -- * Destructuring the Response
    ListComponentOutputsResponse (..),
    newListComponentOutputsResponse,

    -- * Response Lenses
    listComponentOutputsResponse_nextToken,
    listComponentOutputsResponse_httpStatus,
    listComponentOutputsResponse_outputs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListComponentOutputs' smart constructor.
data ListComponentOutputs = ListComponentOutputs'
  { -- | A token that indicates the location of the next output in the array of
    -- outputs, after the list of outputs that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the component whose outputs you want.
    componentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentOutputs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponentOutputs_nextToken' - A token that indicates the location of the next output in the array of
-- outputs, after the list of outputs that was previously requested.
--
-- 'componentName', 'listComponentOutputs_componentName' - The name of the component whose outputs you want.
newListComponentOutputs ::
  -- | 'componentName'
  Prelude.Text ->
  ListComponentOutputs
newListComponentOutputs pComponentName_ =
  ListComponentOutputs'
    { nextToken = Prelude.Nothing,
      componentName = pComponentName_
    }

-- | A token that indicates the location of the next output in the array of
-- outputs, after the list of outputs that was previously requested.
listComponentOutputs_nextToken :: Lens.Lens' ListComponentOutputs (Prelude.Maybe Prelude.Text)
listComponentOutputs_nextToken = Lens.lens (\ListComponentOutputs' {nextToken} -> nextToken) (\s@ListComponentOutputs' {} a -> s {nextToken = a} :: ListComponentOutputs)

-- | The name of the component whose outputs you want.
listComponentOutputs_componentName :: Lens.Lens' ListComponentOutputs Prelude.Text
listComponentOutputs_componentName = Lens.lens (\ListComponentOutputs' {componentName} -> componentName) (\s@ListComponentOutputs' {} a -> s {componentName = a} :: ListComponentOutputs)

instance Core.AWSPager ListComponentOutputs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listComponentOutputsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listComponentOutputsResponse_outputs) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listComponentOutputs_nextToken
          Lens..~ rs
          Lens.^? listComponentOutputsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListComponentOutputs where
  type
    AWSResponse ListComponentOutputs =
      ListComponentOutputsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComponentOutputsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "outputs" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListComponentOutputs where
  hashWithSalt _salt ListComponentOutputs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` componentName

instance Prelude.NFData ListComponentOutputs where
  rnf ListComponentOutputs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf componentName

instance Data.ToHeaders ListComponentOutputs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.ListComponentOutputs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListComponentOutputs where
  toJSON ListComponentOutputs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("componentName" Data..= componentName)
          ]
      )

instance Data.ToPath ListComponentOutputs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListComponentOutputs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListComponentOutputsResponse' smart constructor.
data ListComponentOutputsResponse = ListComponentOutputsResponse'
  { -- | A token that indicates the location of the next output in the array of
    -- outputs, after the list of outputs that was previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of component Infrastructure as Code (IaC) outputs.
    outputs :: [Data.Sensitive Output]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListComponentOutputsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listComponentOutputsResponse_nextToken' - A token that indicates the location of the next output in the array of
-- outputs, after the list of outputs that was previously requested.
--
-- 'httpStatus', 'listComponentOutputsResponse_httpStatus' - The response's http status code.
--
-- 'outputs', 'listComponentOutputsResponse_outputs' - An array of component Infrastructure as Code (IaC) outputs.
newListComponentOutputsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListComponentOutputsResponse
newListComponentOutputsResponse pHttpStatus_ =
  ListComponentOutputsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      outputs = Prelude.mempty
    }

-- | A token that indicates the location of the next output in the array of
-- outputs, after the list of outputs that was previously requested.
listComponentOutputsResponse_nextToken :: Lens.Lens' ListComponentOutputsResponse (Prelude.Maybe Prelude.Text)
listComponentOutputsResponse_nextToken = Lens.lens (\ListComponentOutputsResponse' {nextToken} -> nextToken) (\s@ListComponentOutputsResponse' {} a -> s {nextToken = a} :: ListComponentOutputsResponse)

-- | The response's http status code.
listComponentOutputsResponse_httpStatus :: Lens.Lens' ListComponentOutputsResponse Prelude.Int
listComponentOutputsResponse_httpStatus = Lens.lens (\ListComponentOutputsResponse' {httpStatus} -> httpStatus) (\s@ListComponentOutputsResponse' {} a -> s {httpStatus = a} :: ListComponentOutputsResponse)

-- | An array of component Infrastructure as Code (IaC) outputs.
listComponentOutputsResponse_outputs :: Lens.Lens' ListComponentOutputsResponse [Output]
listComponentOutputsResponse_outputs = Lens.lens (\ListComponentOutputsResponse' {outputs} -> outputs) (\s@ListComponentOutputsResponse' {} a -> s {outputs = a} :: ListComponentOutputsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListComponentOutputsResponse where
  rnf ListComponentOutputsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf outputs
