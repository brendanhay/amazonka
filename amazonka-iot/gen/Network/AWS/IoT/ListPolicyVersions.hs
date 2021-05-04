{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.ListPolicyVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of the specified policy and identifies the default
-- version.
module Network.AWS.IoT.ListPolicyVersions
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListPolicyVersions operation.
--
-- /See:/ 'newListPolicyVersions' smart constructor.
data ListPolicyVersions = ListPolicyVersions'
  { -- | The policy name.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest ListPolicyVersions where
  type
    Rs ListPolicyVersions =
      ListPolicyVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPolicyVersionsResponse'
            Prelude.<$> ( x Prelude..?> "policyVersions"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPolicyVersions

instance Prelude.NFData ListPolicyVersions

instance Prelude.ToHeaders ListPolicyVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ListPolicyVersions where
  toPath ListPolicyVersions' {..} =
    Prelude.mconcat
      ["/policies/", Prelude.toBS policyName, "/version"]

instance Prelude.ToQuery ListPolicyVersions where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
listPolicyVersionsResponse_policyVersions = Lens.lens (\ListPolicyVersionsResponse' {policyVersions} -> policyVersions) (\s@ListPolicyVersionsResponse' {} a -> s {policyVersions = a} :: ListPolicyVersionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listPolicyVersionsResponse_httpStatus :: Lens.Lens' ListPolicyVersionsResponse Prelude.Int
listPolicyVersionsResponse_httpStatus = Lens.lens (\ListPolicyVersionsResponse' {httpStatus} -> httpStatus) (\s@ListPolicyVersionsResponse' {} a -> s {httpStatus = a} :: ListPolicyVersionsResponse)

instance Prelude.NFData ListPolicyVersionsResponse
