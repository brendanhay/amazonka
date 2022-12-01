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
-- Module      : Amazonka.AppStream.ListEntitledApplications
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of entitled applications.
module Amazonka.AppStream.ListEntitledApplications
  ( -- * Creating a Request
    ListEntitledApplications (..),
    newListEntitledApplications,

    -- * Request Lenses
    listEntitledApplications_nextToken,
    listEntitledApplications_maxResults,
    listEntitledApplications_stackName,
    listEntitledApplications_entitlementName,

    -- * Destructuring the Response
    ListEntitledApplicationsResponse (..),
    newListEntitledApplicationsResponse,

    -- * Response Lenses
    listEntitledApplicationsResponse_nextToken,
    listEntitledApplicationsResponse_entitledApplications,
    listEntitledApplicationsResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntitledApplications' smart constructor.
data ListEntitledApplications = ListEntitledApplications'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of each page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The name of the stack with which the entitlement is associated.
    stackName :: Prelude.Text,
    -- | The name of the entitlement.
    entitlementName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitledApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEntitledApplications_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'maxResults', 'listEntitledApplications_maxResults' - The maximum size of each page of results.
--
-- 'stackName', 'listEntitledApplications_stackName' - The name of the stack with which the entitlement is associated.
--
-- 'entitlementName', 'listEntitledApplications_entitlementName' - The name of the entitlement.
newListEntitledApplications ::
  -- | 'stackName'
  Prelude.Text ->
  -- | 'entitlementName'
  Prelude.Text ->
  ListEntitledApplications
newListEntitledApplications
  pStackName_
  pEntitlementName_ =
    ListEntitledApplications'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        stackName = pStackName_,
        entitlementName = pEntitlementName_
      }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listEntitledApplications_nextToken :: Lens.Lens' ListEntitledApplications (Prelude.Maybe Prelude.Text)
listEntitledApplications_nextToken = Lens.lens (\ListEntitledApplications' {nextToken} -> nextToken) (\s@ListEntitledApplications' {} a -> s {nextToken = a} :: ListEntitledApplications)

-- | The maximum size of each page of results.
listEntitledApplications_maxResults :: Lens.Lens' ListEntitledApplications (Prelude.Maybe Prelude.Int)
listEntitledApplications_maxResults = Lens.lens (\ListEntitledApplications' {maxResults} -> maxResults) (\s@ListEntitledApplications' {} a -> s {maxResults = a} :: ListEntitledApplications)

-- | The name of the stack with which the entitlement is associated.
listEntitledApplications_stackName :: Lens.Lens' ListEntitledApplications Prelude.Text
listEntitledApplications_stackName = Lens.lens (\ListEntitledApplications' {stackName} -> stackName) (\s@ListEntitledApplications' {} a -> s {stackName = a} :: ListEntitledApplications)

-- | The name of the entitlement.
listEntitledApplications_entitlementName :: Lens.Lens' ListEntitledApplications Prelude.Text
listEntitledApplications_entitlementName = Lens.lens (\ListEntitledApplications' {entitlementName} -> entitlementName) (\s@ListEntitledApplications' {} a -> s {entitlementName = a} :: ListEntitledApplications)

instance Core.AWSRequest ListEntitledApplications where
  type
    AWSResponse ListEntitledApplications =
      ListEntitledApplicationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntitledApplicationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "EntitledApplications"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEntitledApplications where
  hashWithSalt _salt ListEntitledApplications' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` stackName
      `Prelude.hashWithSalt` entitlementName

instance Prelude.NFData ListEntitledApplications where
  rnf ListEntitledApplications' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf stackName
      `Prelude.seq` Prelude.rnf entitlementName

instance Core.ToHeaders ListEntitledApplications where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.ListEntitledApplications" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEntitledApplications where
  toJSON ListEntitledApplications' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("StackName" Core..= stackName),
            Prelude.Just
              ("EntitlementName" Core..= entitlementName)
          ]
      )

instance Core.ToPath ListEntitledApplications where
  toPath = Prelude.const "/"

instance Core.ToQuery ListEntitledApplications where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntitledApplicationsResponse' smart constructor.
data ListEntitledApplicationsResponse = ListEntitledApplicationsResponse'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The entitled applications.
    entitledApplications :: Prelude.Maybe [EntitledApplication],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntitledApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEntitledApplicationsResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'entitledApplications', 'listEntitledApplicationsResponse_entitledApplications' - The entitled applications.
--
-- 'httpStatus', 'listEntitledApplicationsResponse_httpStatus' - The response's http status code.
newListEntitledApplicationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntitledApplicationsResponse
newListEntitledApplicationsResponse pHttpStatus_ =
  ListEntitledApplicationsResponse'
    { nextToken =
        Prelude.Nothing,
      entitledApplications = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listEntitledApplicationsResponse_nextToken :: Lens.Lens' ListEntitledApplicationsResponse (Prelude.Maybe Prelude.Text)
listEntitledApplicationsResponse_nextToken = Lens.lens (\ListEntitledApplicationsResponse' {nextToken} -> nextToken) (\s@ListEntitledApplicationsResponse' {} a -> s {nextToken = a} :: ListEntitledApplicationsResponse)

-- | The entitled applications.
listEntitledApplicationsResponse_entitledApplications :: Lens.Lens' ListEntitledApplicationsResponse (Prelude.Maybe [EntitledApplication])
listEntitledApplicationsResponse_entitledApplications = Lens.lens (\ListEntitledApplicationsResponse' {entitledApplications} -> entitledApplications) (\s@ListEntitledApplicationsResponse' {} a -> s {entitledApplications = a} :: ListEntitledApplicationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEntitledApplicationsResponse_httpStatus :: Lens.Lens' ListEntitledApplicationsResponse Prelude.Int
listEntitledApplicationsResponse_httpStatus = Lens.lens (\ListEntitledApplicationsResponse' {httpStatus} -> httpStatus) (\s@ListEntitledApplicationsResponse' {} a -> s {httpStatus = a} :: ListEntitledApplicationsResponse)

instance
  Prelude.NFData
    ListEntitledApplicationsResponse
  where
  rnf ListEntitledApplicationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf entitledApplications
      `Prelude.seq` Prelude.rnf httpStatus
