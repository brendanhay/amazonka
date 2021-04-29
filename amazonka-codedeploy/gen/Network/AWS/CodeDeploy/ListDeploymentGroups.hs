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
-- Module      : Network.AWS.CodeDeploy.ListDeploymentGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment groups for an application registered with the IAM
-- user or AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentGroups
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
    listDeploymentGroupsResponse_nextToken,
    listDeploymentGroupsResponse_deploymentGroups,
    listDeploymentGroupsResponse_applicationName,
    listDeploymentGroupsResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListDeploymentGroups@ operation.
--
-- /See:/ 'newListDeploymentGroups' smart constructor.
data ListDeploymentGroups = ListDeploymentGroups'
  { -- | An identifier returned from the previous list deployment groups call. It
    -- can be used to return the next set of deployment groups in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'applicationName', 'listDeploymentGroups_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
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

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
listDeploymentGroups_applicationName :: Lens.Lens' ListDeploymentGroups Prelude.Text
listDeploymentGroups_applicationName = Lens.lens (\ListDeploymentGroups' {applicationName} -> applicationName) (\s@ListDeploymentGroups' {} a -> s {applicationName = a} :: ListDeploymentGroups)

instance Pager.AWSPager ListDeploymentGroups where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listDeploymentGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listDeploymentGroupsResponse_deploymentGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listDeploymentGroups_nextToken
          Lens..~ rs
          Lens.^? listDeploymentGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListDeploymentGroups where
  type
    Rs ListDeploymentGroups =
      ListDeploymentGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentGroupsResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "deploymentGroups"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "applicationName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeploymentGroups

instance Prelude.NFData ListDeploymentGroups

instance Prelude.ToHeaders ListDeploymentGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.ListDeploymentGroups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListDeploymentGroups where
  toJSON ListDeploymentGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            Prelude.Just
              ("applicationName" Prelude..= applicationName)
          ]
      )

instance Prelude.ToPath ListDeploymentGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListDeploymentGroups where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @ListDeploymentGroups@ operation.
--
-- /See:/ 'newListDeploymentGroupsResponse' smart constructor.
data ListDeploymentGroupsResponse = ListDeploymentGroupsResponse'
  { -- | If a large amount of information is returned, an identifier is also
    -- returned. It can be used in a subsequent list deployment groups call to
    -- return the next set of deployment groups in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of deployment group names.
    deploymentGroups :: Prelude.Maybe [Prelude.Text],
    -- | The application name.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentGroupsResponse_nextToken' - If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployment groups call to
-- return the next set of deployment groups in the list.
--
-- 'deploymentGroups', 'listDeploymentGroupsResponse_deploymentGroups' - A list of deployment group names.
--
-- 'applicationName', 'listDeploymentGroupsResponse_applicationName' - The application name.
--
-- 'httpStatus', 'listDeploymentGroupsResponse_httpStatus' - The response's http status code.
newListDeploymentGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentGroupsResponse
newListDeploymentGroupsResponse pHttpStatus_ =
  ListDeploymentGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      deploymentGroups = Prelude.Nothing,
      applicationName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If a large amount of information is returned, an identifier is also
-- returned. It can be used in a subsequent list deployment groups call to
-- return the next set of deployment groups in the list.
listDeploymentGroupsResponse_nextToken :: Lens.Lens' ListDeploymentGroupsResponse (Prelude.Maybe Prelude.Text)
listDeploymentGroupsResponse_nextToken = Lens.lens (\ListDeploymentGroupsResponse' {nextToken} -> nextToken) (\s@ListDeploymentGroupsResponse' {} a -> s {nextToken = a} :: ListDeploymentGroupsResponse)

-- | A list of deployment group names.
listDeploymentGroupsResponse_deploymentGroups :: Lens.Lens' ListDeploymentGroupsResponse (Prelude.Maybe [Prelude.Text])
listDeploymentGroupsResponse_deploymentGroups = Lens.lens (\ListDeploymentGroupsResponse' {deploymentGroups} -> deploymentGroups) (\s@ListDeploymentGroupsResponse' {} a -> s {deploymentGroups = a} :: ListDeploymentGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The application name.
listDeploymentGroupsResponse_applicationName :: Lens.Lens' ListDeploymentGroupsResponse (Prelude.Maybe Prelude.Text)
listDeploymentGroupsResponse_applicationName = Lens.lens (\ListDeploymentGroupsResponse' {applicationName} -> applicationName) (\s@ListDeploymentGroupsResponse' {} a -> s {applicationName = a} :: ListDeploymentGroupsResponse)

-- | The response's http status code.
listDeploymentGroupsResponse_httpStatus :: Lens.Lens' ListDeploymentGroupsResponse Prelude.Int
listDeploymentGroupsResponse_httpStatus = Lens.lens (\ListDeploymentGroupsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentGroupsResponse' {} a -> s {httpStatus = a} :: ListDeploymentGroupsResponse)

instance Prelude.NFData ListDeploymentGroupsResponse
