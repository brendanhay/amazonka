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
-- Module      : Amazonka.Grafana.DescribeWorkspace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays information about one Amazon Managed Grafana workspace.
module Amazonka.Grafana.DescribeWorkspace
  ( -- * Creating a Request
    DescribeWorkspace (..),
    newDescribeWorkspace,

    -- * Request Lenses
    describeWorkspace_workspaceId,

    -- * Destructuring the Response
    DescribeWorkspaceResponse (..),
    newDescribeWorkspaceResponse,

    -- * Response Lenses
    describeWorkspaceResponse_httpStatus,
    describeWorkspaceResponse_workspace,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Grafana.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeWorkspace' smart constructor.
data DescribeWorkspace = DescribeWorkspace'
  { -- | The ID of the workspace to display information about.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'describeWorkspace_workspaceId' - The ID of the workspace to display information about.
newDescribeWorkspace ::
  -- | 'workspaceId'
  Prelude.Text ->
  DescribeWorkspace
newDescribeWorkspace pWorkspaceId_ =
  DescribeWorkspace' {workspaceId = pWorkspaceId_}

-- | The ID of the workspace to display information about.
describeWorkspace_workspaceId :: Lens.Lens' DescribeWorkspace Prelude.Text
describeWorkspace_workspaceId = Lens.lens (\DescribeWorkspace' {workspaceId} -> workspaceId) (\s@DescribeWorkspace' {} a -> s {workspaceId = a} :: DescribeWorkspace)

instance Core.AWSRequest DescribeWorkspace where
  type
    AWSResponse DescribeWorkspace =
      DescribeWorkspaceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workspace")
      )

instance Prelude.Hashable DescribeWorkspace where
  hashWithSalt _salt DescribeWorkspace' {..} =
    _salt `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData DescribeWorkspace where
  rnf DescribeWorkspace' {..} = Prelude.rnf workspaceId

instance Data.ToHeaders DescribeWorkspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeWorkspace where
  toPath DescribeWorkspace' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId]

instance Data.ToQuery DescribeWorkspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkspaceResponse' smart constructor.
data DescribeWorkspaceResponse = DescribeWorkspaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure containing information about the workspace.
    workspace :: WorkspaceDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeWorkspaceResponse_httpStatus' - The response's http status code.
--
-- 'workspace', 'describeWorkspaceResponse_workspace' - A structure containing information about the workspace.
newDescribeWorkspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workspace'
  WorkspaceDescription ->
  DescribeWorkspaceResponse
newDescribeWorkspaceResponse pHttpStatus_ pWorkspace_ =
  DescribeWorkspaceResponse'
    { httpStatus =
        pHttpStatus_,
      workspace = pWorkspace_
    }

-- | The response's http status code.
describeWorkspaceResponse_httpStatus :: Lens.Lens' DescribeWorkspaceResponse Prelude.Int
describeWorkspaceResponse_httpStatus = Lens.lens (\DescribeWorkspaceResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspaceResponse' {} a -> s {httpStatus = a} :: DescribeWorkspaceResponse)

-- | A structure containing information about the workspace.
describeWorkspaceResponse_workspace :: Lens.Lens' DescribeWorkspaceResponse WorkspaceDescription
describeWorkspaceResponse_workspace = Lens.lens (\DescribeWorkspaceResponse' {workspace} -> workspace) (\s@DescribeWorkspaceResponse' {} a -> s {workspace = a} :: DescribeWorkspaceResponse)

instance Prelude.NFData DescribeWorkspaceResponse where
  rnf DescribeWorkspaceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workspace
