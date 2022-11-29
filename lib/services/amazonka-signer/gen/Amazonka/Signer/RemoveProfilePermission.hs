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
-- Module      : Amazonka.Signer.RemoveProfilePermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes cross-account permissions from a signing profile.
module Amazonka.Signer.RemoveProfilePermission
  ( -- * Creating a Request
    RemoveProfilePermission (..),
    newRemoveProfilePermission,

    -- * Request Lenses
    removeProfilePermission_revisionId,
    removeProfilePermission_profileName,
    removeProfilePermission_statementId,

    -- * Destructuring the Response
    RemoveProfilePermissionResponse (..),
    newRemoveProfilePermissionResponse,

    -- * Response Lenses
    removeProfilePermissionResponse_revisionId,
    removeProfilePermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Signer.Types

-- | /See:/ 'newRemoveProfilePermission' smart constructor.
data RemoveProfilePermission = RemoveProfilePermission'
  { -- | An identifier for the current revision of the signing profile
    -- permissions.
    revisionId :: Prelude.Text,
    -- | A human-readable name for the signing profile with permissions to be
    -- removed.
    profileName :: Prelude.Text,
    -- | A unique identifier for the cross-account permissions statement.
    statementId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveProfilePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'removeProfilePermission_revisionId' - An identifier for the current revision of the signing profile
-- permissions.
--
-- 'profileName', 'removeProfilePermission_profileName' - A human-readable name for the signing profile with permissions to be
-- removed.
--
-- 'statementId', 'removeProfilePermission_statementId' - A unique identifier for the cross-account permissions statement.
newRemoveProfilePermission ::
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'profileName'
  Prelude.Text ->
  -- | 'statementId'
  Prelude.Text ->
  RemoveProfilePermission
newRemoveProfilePermission
  pRevisionId_
  pProfileName_
  pStatementId_ =
    RemoveProfilePermission'
      { revisionId = pRevisionId_,
        profileName = pProfileName_,
        statementId = pStatementId_
      }

-- | An identifier for the current revision of the signing profile
-- permissions.
removeProfilePermission_revisionId :: Lens.Lens' RemoveProfilePermission Prelude.Text
removeProfilePermission_revisionId = Lens.lens (\RemoveProfilePermission' {revisionId} -> revisionId) (\s@RemoveProfilePermission' {} a -> s {revisionId = a} :: RemoveProfilePermission)

-- | A human-readable name for the signing profile with permissions to be
-- removed.
removeProfilePermission_profileName :: Lens.Lens' RemoveProfilePermission Prelude.Text
removeProfilePermission_profileName = Lens.lens (\RemoveProfilePermission' {profileName} -> profileName) (\s@RemoveProfilePermission' {} a -> s {profileName = a} :: RemoveProfilePermission)

-- | A unique identifier for the cross-account permissions statement.
removeProfilePermission_statementId :: Lens.Lens' RemoveProfilePermission Prelude.Text
removeProfilePermission_statementId = Lens.lens (\RemoveProfilePermission' {statementId} -> statementId) (\s@RemoveProfilePermission' {} a -> s {statementId = a} :: RemoveProfilePermission)

instance Core.AWSRequest RemoveProfilePermission where
  type
    AWSResponse RemoveProfilePermission =
      RemoveProfilePermissionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RemoveProfilePermissionResponse'
            Prelude.<$> (x Core..?> "revisionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveProfilePermission where
  hashWithSalt _salt RemoveProfilePermission' {..} =
    _salt `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` statementId

instance Prelude.NFData RemoveProfilePermission where
  rnf RemoveProfilePermission' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf statementId

instance Core.ToHeaders RemoveProfilePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath RemoveProfilePermission where
  toPath RemoveProfilePermission' {..} =
    Prelude.mconcat
      [ "/signing-profiles/",
        Core.toBS profileName,
        "/permissions/",
        Core.toBS statementId
      ]

instance Core.ToQuery RemoveProfilePermission where
  toQuery RemoveProfilePermission' {..} =
    Prelude.mconcat ["revisionId" Core.=: revisionId]

-- | /See:/ 'newRemoveProfilePermissionResponse' smart constructor.
data RemoveProfilePermissionResponse = RemoveProfilePermissionResponse'
  { -- | An identifier for the current revision of the profile permissions.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveProfilePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'removeProfilePermissionResponse_revisionId' - An identifier for the current revision of the profile permissions.
--
-- 'httpStatus', 'removeProfilePermissionResponse_httpStatus' - The response's http status code.
newRemoveProfilePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveProfilePermissionResponse
newRemoveProfilePermissionResponse pHttpStatus_ =
  RemoveProfilePermissionResponse'
    { revisionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier for the current revision of the profile permissions.
removeProfilePermissionResponse_revisionId :: Lens.Lens' RemoveProfilePermissionResponse (Prelude.Maybe Prelude.Text)
removeProfilePermissionResponse_revisionId = Lens.lens (\RemoveProfilePermissionResponse' {revisionId} -> revisionId) (\s@RemoveProfilePermissionResponse' {} a -> s {revisionId = a} :: RemoveProfilePermissionResponse)

-- | The response's http status code.
removeProfilePermissionResponse_httpStatus :: Lens.Lens' RemoveProfilePermissionResponse Prelude.Int
removeProfilePermissionResponse_httpStatus = Lens.lens (\RemoveProfilePermissionResponse' {httpStatus} -> httpStatus) (\s@RemoveProfilePermissionResponse' {} a -> s {httpStatus = a} :: RemoveProfilePermissionResponse)

instance
  Prelude.NFData
    RemoveProfilePermissionResponse
  where
  rnf RemoveProfilePermissionResponse' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf httpStatus
