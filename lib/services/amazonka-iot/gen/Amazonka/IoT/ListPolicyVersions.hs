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
-- Module      : Amazonka.IoT.ListPolicyVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of the specified policy and identifies the default
-- version.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListPolicyVersions>
-- action.
module Amazonka.IoT.ListPolicyVersions
  ( -- * Creating a Request
    ListPolicyVersions (..),
    newListPolicyVersions,

    -- * Request Lenses
    listPolicyVersions_policyName,

    -- * Destructuring the Response
    ListPolicyVersionsResponse (..),
    newListPolicyVersionsResponse,

    -- * Response Lenses
    listPolicyVersionsResponse_policyVersions,
    listPolicyVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the ListPolicyVersions operation.
--
-- /See:/ 'newListPolicyVersions' smart constructor.
data ListPolicyVersions = ListPolicyVersions'
  { -- | The policy name.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'listPolicyVersions_policyName' - The policy name.
newListPolicyVersions ::
  -- | 'policyName'
  Prelude.Text ->
  ListPolicyVersions
newListPolicyVersions pPolicyName_ =
  ListPolicyVersions' {policyName = pPolicyName_}

-- | The policy name.
listPolicyVersions_policyName :: Lens.Lens' ListPolicyVersions Prelude.Text
listPolicyVersions_policyName = Lens.lens (\ListPolicyVersions' {policyName} -> policyName) (\s@ListPolicyVersions' {} a -> s {policyName = a} :: ListPolicyVersions)

instance Core.AWSRequest ListPolicyVersions where
  type
    AWSResponse ListPolicyVersions =
      ListPolicyVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPolicyVersionsResponse'
            Prelude.<$> (x Data..?> "policyVersions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPolicyVersions where
  hashWithSalt _salt ListPolicyVersions' {..} =
    _salt `Prelude.hashWithSalt` policyName

instance Prelude.NFData ListPolicyVersions where
  rnf ListPolicyVersions' {..} = Prelude.rnf policyName

instance Data.ToHeaders ListPolicyVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListPolicyVersions where
  toPath ListPolicyVersions' {..} =
    Prelude.mconcat
      ["/policies/", Data.toBS policyName, "/version"]

instance Data.ToQuery ListPolicyVersions where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the ListPolicyVersions operation.
--
-- /See:/ 'newListPolicyVersionsResponse' smart constructor.
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
  { -- | The policy versions.
    policyVersions :: Prelude.Maybe [PolicyVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPolicyVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyVersions', 'listPolicyVersionsResponse_policyVersions' - The policy versions.
--
-- 'httpStatus', 'listPolicyVersionsResponse_httpStatus' - The response's http status code.
newListPolicyVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPolicyVersionsResponse
newListPolicyVersionsResponse pHttpStatus_ =
  ListPolicyVersionsResponse'
    { policyVersions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy versions.
listPolicyVersionsResponse_policyVersions :: Lens.Lens' ListPolicyVersionsResponse (Prelude.Maybe [PolicyVersion])
listPolicyVersionsResponse_policyVersions = Lens.lens (\ListPolicyVersionsResponse' {policyVersions} -> policyVersions) (\s@ListPolicyVersionsResponse' {} a -> s {policyVersions = a} :: ListPolicyVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPolicyVersionsResponse_httpStatus :: Lens.Lens' ListPolicyVersionsResponse Prelude.Int
listPolicyVersionsResponse_httpStatus = Lens.lens (\ListPolicyVersionsResponse' {httpStatus} -> httpStatus) (\s@ListPolicyVersionsResponse' {} a -> s {httpStatus = a} :: ListPolicyVersionsResponse)

instance Prelude.NFData ListPolicyVersionsResponse where
  rnf ListPolicyVersionsResponse' {..} =
    Prelude.rnf policyVersions `Prelude.seq`
      Prelude.rnf httpStatus
