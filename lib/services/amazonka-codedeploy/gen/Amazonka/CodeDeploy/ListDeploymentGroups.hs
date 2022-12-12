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
-- Module      : Amazonka.CodeDeploy.ListDeploymentGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment groups for an application registered with the IAM
-- user or Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.CodeDeploy.ListDeploymentGroups
  ( -- * Creating a Request
    ListDeploymentGroups (..),
    newListDeploymentGroups,

    -- * Request Lenses
    listDeploymentGroups_nextToken,
    listDeploymentGroups_applicationName,

    -- * Destructuring the Response
    ListDeploymentGroupsResponse (..),
    newListDeploymentGroupsResponse,

    -- * Response Lenses
    listDeploymentGroupsResponse_applicationName,
    listDeploymentGroupsResponse_deploymentGroups,
    listDeploymentGroupsResponse_nextToken,
    listDeploymentGroupsResponse_httpStatus,
  )
where

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ListDeploymentGroups@ operation.
--
-- /See:/ 'newListDeploymentGroups' smart constructor.
data ListDeploymentGroups = ListDeploymentGroups'
  { -- | An identifier returned from the previous list deployment groups call. It
    -- can be used to return the next set of deployment groups in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of an CodeDeploy application associated with the IAM user or
    -- Amazon Web Services account.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentGroups_nextToken' - An identifier returned from the previous list deployment groups call. It
-- can be used to return the next set of deployment groups in the list.
--
-- 'applicationName', 'listDeploymentGroups_applicationName' - The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
newListDeploymentGroups ::
  -- | 'applicationName'
  Prelude.Text ->
  ListDeploymentGroups
newListDeploymentGroups pApplicationName_ =
  ListDeploymentGroups'
    { nextToken = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | An identifier returned from the previous list deployment groups call. It
-- can be used to return the next set of deployment groups in the list.
listDeploymentGroups_nextToken :: Lens.Lens' ListDeploymentGroups (Prelude.Maybe Prelude.Text)
listDeploymentGroups_nextToken = Lens.lens (\ListDeploymentGroups' {nextToken} -> nextToken) (\s@ListDeploymentGroups' {} a -> s {nextToken = a} :: ListDeploymentGroups)

-- | The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
listDeploymentGroups_applicationName :: Lens.Lens' ListDeploymentGroups Prelude.Text
listDeploymentGroups_applicationName = Lens.lens (\ListDeploymentGroups' {applicationName} -> applicationName) (\s@ListDeploymentGroups' {} a -> s {applicationName = a} :: ListDeploymentGroups)

instance Core.AWSPager ListDeploymentGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeploymentGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeploymentGroupsResponse_deploymentGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDeploymentGroups_nextToken
          Lens..~ rs
          Lens.^? listDeploymentGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDeploymentGroups where
  type
    AWSResponse ListDeploymentGroups =
      ListDeploymentGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentGroupsResponse'
            Prelude.<$> (x Data..?> "applicationName")
            Prelude.<*> ( x Data..?> "deploymentGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeploymentGroups where
  hashWithSalt _salt ListDeploymentGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` applicationName

instance Prelude.NFData ListDeploymentGroups where
  rnf ListDeploymentGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf applicationName

instance Data.ToHeaders ListDeploymentGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.ListDeploymentGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDeploymentGroups where
  toJSON ListDeploymentGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("applicationName" Data..= applicationName)
          ]
      )

instance Data.ToPath ListDeploymentGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDeploymentGroups where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListDeploymentGroups@ operation.
--
-- /See:/ 'newListDeploymentGroupsResponse' smart constructor.
data ListDeploymentGroupsResponse = ListDeploymentGroupsResponse'
  { -- | The application name.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | A list of deployment group names.
    deploymentGroups :: Prelude.Maybe [Prelude.Text],
    -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list deployment groups call to
    -- return the next set of deployment groups in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'listDeploymentGroupsResponse_applicationName' - The application name.
--
-- 'deploymentGroups', 'listDeploymentGroupsResponse_deploymentGroups' - A list of deployment group names.
--
-- 'nextToken', 'listDeploymentGroupsResponse_nextToken' - If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployment groups call to
-- return the next set of deployment groups in the list.
--
-- 'httpStatus', 'listDeploymentGroupsResponse_httpStatus' - The response's http status code.
newListDeploymentGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentGroupsResponse
newListDeploymentGroupsResponse pHttpStatus_ =
  ListDeploymentGroupsResponse'
    { applicationName =
        Prelude.Nothing,
      deploymentGroups = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application name.
listDeploymentGroupsResponse_applicationName :: Lens.Lens' ListDeploymentGroupsResponse (Prelude.Maybe Prelude.Text)
listDeploymentGroupsResponse_applicationName = Lens.lens (\ListDeploymentGroupsResponse' {applicationName} -> applicationName) (\s@ListDeploymentGroupsResponse' {} a -> s {applicationName = a} :: ListDeploymentGroupsResponse)

-- | A list of deployment group names.
listDeploymentGroupsResponse_deploymentGroups :: Lens.Lens' ListDeploymentGroupsResponse (Prelude.Maybe [Prelude.Text])
listDeploymentGroupsResponse_deploymentGroups = Lens.lens (\ListDeploymentGroupsResponse' {deploymentGroups} -> deploymentGroups) (\s@ListDeploymentGroupsResponse' {} a -> s {deploymentGroups = a} :: ListDeploymentGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployment groups call to
-- return the next set of deployment groups in the list.
listDeploymentGroupsResponse_nextToken :: Lens.Lens' ListDeploymentGroupsResponse (Prelude.Maybe Prelude.Text)
listDeploymentGroupsResponse_nextToken = Lens.lens (\ListDeploymentGroupsResponse' {nextToken} -> nextToken) (\s@ListDeploymentGroupsResponse' {} a -> s {nextToken = a} :: ListDeploymentGroupsResponse)

-- | The response's http status code.
listDeploymentGroupsResponse_httpStatus :: Lens.Lens' ListDeploymentGroupsResponse Prelude.Int
listDeploymentGroupsResponse_httpStatus = Lens.lens (\ListDeploymentGroupsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentGroupsResponse' {} a -> s {httpStatus = a} :: ListDeploymentGroupsResponse)

instance Prelude.NFData ListDeploymentGroupsResponse where
  rnf ListDeploymentGroupsResponse' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf deploymentGroups
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
