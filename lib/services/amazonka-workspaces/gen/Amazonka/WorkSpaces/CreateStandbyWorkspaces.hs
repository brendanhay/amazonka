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
-- Module      : Amazonka.WorkSpaces.CreateStandbyWorkspaces
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Standby WorkSpace in a secondary region.
module Amazonka.WorkSpaces.CreateStandbyWorkspaces
  ( -- * Creating a Request
    CreateStandbyWorkspaces (..),
    newCreateStandbyWorkspaces,

    -- * Request Lenses
    createStandbyWorkspaces_primaryRegion,
    createStandbyWorkspaces_standbyWorkspaces,

    -- * Destructuring the Response
    CreateStandbyWorkspacesResponse (..),
    newCreateStandbyWorkspacesResponse,

    -- * Response Lenses
    createStandbyWorkspacesResponse_failedStandbyRequests,
    createStandbyWorkspacesResponse_pendingStandbyRequests,
    createStandbyWorkspacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newCreateStandbyWorkspaces' smart constructor.
data CreateStandbyWorkspaces = CreateStandbyWorkspaces'
  { -- | The Region of the primary WorkSpace.
    primaryRegion :: Prelude.Text,
    -- | Information about the Standby WorkSpace to be created.
    standbyWorkspaces :: [StandbyWorkspace]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStandbyWorkspaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primaryRegion', 'createStandbyWorkspaces_primaryRegion' - The Region of the primary WorkSpace.
--
-- 'standbyWorkspaces', 'createStandbyWorkspaces_standbyWorkspaces' - Information about the Standby WorkSpace to be created.
newCreateStandbyWorkspaces ::
  -- | 'primaryRegion'
  Prelude.Text ->
  CreateStandbyWorkspaces
newCreateStandbyWorkspaces pPrimaryRegion_ =
  CreateStandbyWorkspaces'
    { primaryRegion =
        pPrimaryRegion_,
      standbyWorkspaces = Prelude.mempty
    }

-- | The Region of the primary WorkSpace.
createStandbyWorkspaces_primaryRegion :: Lens.Lens' CreateStandbyWorkspaces Prelude.Text
createStandbyWorkspaces_primaryRegion = Lens.lens (\CreateStandbyWorkspaces' {primaryRegion} -> primaryRegion) (\s@CreateStandbyWorkspaces' {} a -> s {primaryRegion = a} :: CreateStandbyWorkspaces)

-- | Information about the Standby WorkSpace to be created.
createStandbyWorkspaces_standbyWorkspaces :: Lens.Lens' CreateStandbyWorkspaces [StandbyWorkspace]
createStandbyWorkspaces_standbyWorkspaces = Lens.lens (\CreateStandbyWorkspaces' {standbyWorkspaces} -> standbyWorkspaces) (\s@CreateStandbyWorkspaces' {} a -> s {standbyWorkspaces = a} :: CreateStandbyWorkspaces) Prelude.. Lens.coerced

instance Core.AWSRequest CreateStandbyWorkspaces where
  type
    AWSResponse CreateStandbyWorkspaces =
      CreateStandbyWorkspacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateStandbyWorkspacesResponse'
            Prelude.<$> ( x Data..?> "FailedStandbyRequests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "PendingStandbyRequests"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateStandbyWorkspaces where
  hashWithSalt _salt CreateStandbyWorkspaces' {..} =
    _salt `Prelude.hashWithSalt` primaryRegion
      `Prelude.hashWithSalt` standbyWorkspaces

instance Prelude.NFData CreateStandbyWorkspaces where
  rnf CreateStandbyWorkspaces' {..} =
    Prelude.rnf primaryRegion
      `Prelude.seq` Prelude.rnf standbyWorkspaces

instance Data.ToHeaders CreateStandbyWorkspaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.CreateStandbyWorkspaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateStandbyWorkspaces where
  toJSON CreateStandbyWorkspaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PrimaryRegion" Data..= primaryRegion),
            Prelude.Just
              ("StandbyWorkspaces" Data..= standbyWorkspaces)
          ]
      )

instance Data.ToPath CreateStandbyWorkspaces where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateStandbyWorkspaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateStandbyWorkspacesResponse' smart constructor.
data CreateStandbyWorkspacesResponse = CreateStandbyWorkspacesResponse'
  { -- | Information about the Standby WorkSpace that could not be created.
    failedStandbyRequests :: Prelude.Maybe [FailedCreateStandbyWorkspacesRequest],
    -- | Information about the Standby WorkSpace that was created.
    pendingStandbyRequests :: Prelude.Maybe [PendingCreateStandbyWorkspacesRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateStandbyWorkspacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedStandbyRequests', 'createStandbyWorkspacesResponse_failedStandbyRequests' - Information about the Standby WorkSpace that could not be created.
--
-- 'pendingStandbyRequests', 'createStandbyWorkspacesResponse_pendingStandbyRequests' - Information about the Standby WorkSpace that was created.
--
-- 'httpStatus', 'createStandbyWorkspacesResponse_httpStatus' - The response's http status code.
newCreateStandbyWorkspacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateStandbyWorkspacesResponse
newCreateStandbyWorkspacesResponse pHttpStatus_ =
  CreateStandbyWorkspacesResponse'
    { failedStandbyRequests =
        Prelude.Nothing,
      pendingStandbyRequests = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Standby WorkSpace that could not be created.
createStandbyWorkspacesResponse_failedStandbyRequests :: Lens.Lens' CreateStandbyWorkspacesResponse (Prelude.Maybe [FailedCreateStandbyWorkspacesRequest])
createStandbyWorkspacesResponse_failedStandbyRequests = Lens.lens (\CreateStandbyWorkspacesResponse' {failedStandbyRequests} -> failedStandbyRequests) (\s@CreateStandbyWorkspacesResponse' {} a -> s {failedStandbyRequests = a} :: CreateStandbyWorkspacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the Standby WorkSpace that was created.
createStandbyWorkspacesResponse_pendingStandbyRequests :: Lens.Lens' CreateStandbyWorkspacesResponse (Prelude.Maybe [PendingCreateStandbyWorkspacesRequest])
createStandbyWorkspacesResponse_pendingStandbyRequests = Lens.lens (\CreateStandbyWorkspacesResponse' {pendingStandbyRequests} -> pendingStandbyRequests) (\s@CreateStandbyWorkspacesResponse' {} a -> s {pendingStandbyRequests = a} :: CreateStandbyWorkspacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createStandbyWorkspacesResponse_httpStatus :: Lens.Lens' CreateStandbyWorkspacesResponse Prelude.Int
createStandbyWorkspacesResponse_httpStatus = Lens.lens (\CreateStandbyWorkspacesResponse' {httpStatus} -> httpStatus) (\s@CreateStandbyWorkspacesResponse' {} a -> s {httpStatus = a} :: CreateStandbyWorkspacesResponse)

instance
  Prelude.NFData
    CreateStandbyWorkspacesResponse
  where
  rnf CreateStandbyWorkspacesResponse' {..} =
    Prelude.rnf failedStandbyRequests
      `Prelude.seq` Prelude.rnf pendingStandbyRequests
      `Prelude.seq` Prelude.rnf httpStatus
