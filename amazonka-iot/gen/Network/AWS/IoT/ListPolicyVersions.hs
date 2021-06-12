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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListPolicyVersions operation.
--
-- /See:/ 'newListPolicyVersions' smart constructor.
data ListPolicyVersions = ListPolicyVersions'
  { -- | The policy name.
    policyName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListPolicyVersions
newListPolicyVersions pPolicyName_ =
  ListPolicyVersions' {policyName = pPolicyName_}

-- | The policy name.
listPolicyVersions_policyName :: Lens.Lens' ListPolicyVersions Core.Text
listPolicyVersions_policyName = Lens.lens (\ListPolicyVersions' {policyName} -> policyName) (\s@ListPolicyVersions' {} a -> s {policyName = a} :: ListPolicyVersions)

instance Core.AWSRequest ListPolicyVersions where
  type
    AWSResponse ListPolicyVersions =
      ListPolicyVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPolicyVersionsResponse'
            Core.<$> (x Core..?> "policyVersions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListPolicyVersions

instance Core.NFData ListPolicyVersions

instance Core.ToHeaders ListPolicyVersions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListPolicyVersions where
  toPath ListPolicyVersions' {..} =
    Core.mconcat
      ["/policies/", Core.toBS policyName, "/version"]

instance Core.ToQuery ListPolicyVersions where
  toQuery = Core.const Core.mempty

-- | The output from the ListPolicyVersions operation.
--
-- /See:/ 'newListPolicyVersionsResponse' smart constructor.
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
  { -- | The policy versions.
    policyVersions :: Core.Maybe [PolicyVersion],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListPolicyVersionsResponse
newListPolicyVersionsResponse pHttpStatus_ =
  ListPolicyVersionsResponse'
    { policyVersions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy versions.
listPolicyVersionsResponse_policyVersions :: Lens.Lens' ListPolicyVersionsResponse (Core.Maybe [PolicyVersion])
listPolicyVersionsResponse_policyVersions = Lens.lens (\ListPolicyVersionsResponse' {policyVersions} -> policyVersions) (\s@ListPolicyVersionsResponse' {} a -> s {policyVersions = a} :: ListPolicyVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listPolicyVersionsResponse_httpStatus :: Lens.Lens' ListPolicyVersionsResponse Core.Int
listPolicyVersionsResponse_httpStatus = Lens.lens (\ListPolicyVersionsResponse' {httpStatus} -> httpStatus) (\s@ListPolicyVersionsResponse' {} a -> s {httpStatus = a} :: ListPolicyVersionsResponse)

instance Core.NFData ListPolicyVersionsResponse
