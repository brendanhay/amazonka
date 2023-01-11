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
-- Module      : Amazonka.QuickSight.DeleteFolderMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an asset, such as a dashboard, analysis, or dataset, from a
-- folder.
module Amazonka.QuickSight.DeleteFolderMembership
  ( -- * Creating a Request
    DeleteFolderMembership (..),
    newDeleteFolderMembership,

    -- * Request Lenses
    deleteFolderMembership_awsAccountId,
    deleteFolderMembership_folderId,
    deleteFolderMembership_memberId,
    deleteFolderMembership_memberType,

    -- * Destructuring the Response
    DeleteFolderMembershipResponse (..),
    newDeleteFolderMembershipResponse,

    -- * Response Lenses
    deleteFolderMembershipResponse_requestId,
    deleteFolderMembershipResponse_status,
    deleteFolderMembershipResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFolderMembership' smart constructor.
data DeleteFolderMembership = DeleteFolderMembership'
  { -- | The ID for the Amazon Web Services account that contains the folder.
    awsAccountId :: Prelude.Text,
    -- | The Folder ID.
    folderId :: Prelude.Text,
    -- | The ID of the asset (the dashboard, analysis, or dataset) that you want
    -- to delete.
    memberId :: Prelude.Text,
    -- | The type of the member, including @DASHBOARD@, @ANALYSIS@, and @DATASET@
    memberType :: MemberType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolderMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'deleteFolderMembership_awsAccountId' - The ID for the Amazon Web Services account that contains the folder.
--
-- 'folderId', 'deleteFolderMembership_folderId' - The Folder ID.
--
-- 'memberId', 'deleteFolderMembership_memberId' - The ID of the asset (the dashboard, analysis, or dataset) that you want
-- to delete.
--
-- 'memberType', 'deleteFolderMembership_memberType' - The type of the member, including @DASHBOARD@, @ANALYSIS@, and @DATASET@
newDeleteFolderMembership ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  -- | 'memberType'
  MemberType ->
  DeleteFolderMembership
newDeleteFolderMembership
  pAwsAccountId_
  pFolderId_
  pMemberId_
  pMemberType_ =
    DeleteFolderMembership'
      { awsAccountId =
          pAwsAccountId_,
        folderId = pFolderId_,
        memberId = pMemberId_,
        memberType = pMemberType_
      }

-- | The ID for the Amazon Web Services account that contains the folder.
deleteFolderMembership_awsAccountId :: Lens.Lens' DeleteFolderMembership Prelude.Text
deleteFolderMembership_awsAccountId = Lens.lens (\DeleteFolderMembership' {awsAccountId} -> awsAccountId) (\s@DeleteFolderMembership' {} a -> s {awsAccountId = a} :: DeleteFolderMembership)

-- | The Folder ID.
deleteFolderMembership_folderId :: Lens.Lens' DeleteFolderMembership Prelude.Text
deleteFolderMembership_folderId = Lens.lens (\DeleteFolderMembership' {folderId} -> folderId) (\s@DeleteFolderMembership' {} a -> s {folderId = a} :: DeleteFolderMembership)

-- | The ID of the asset (the dashboard, analysis, or dataset) that you want
-- to delete.
deleteFolderMembership_memberId :: Lens.Lens' DeleteFolderMembership Prelude.Text
deleteFolderMembership_memberId = Lens.lens (\DeleteFolderMembership' {memberId} -> memberId) (\s@DeleteFolderMembership' {} a -> s {memberId = a} :: DeleteFolderMembership)

-- | The type of the member, including @DASHBOARD@, @ANALYSIS@, and @DATASET@
deleteFolderMembership_memberType :: Lens.Lens' DeleteFolderMembership MemberType
deleteFolderMembership_memberType = Lens.lens (\DeleteFolderMembership' {memberType} -> memberType) (\s@DeleteFolderMembership' {} a -> s {memberType = a} :: DeleteFolderMembership)

instance Core.AWSRequest DeleteFolderMembership where
  type
    AWSResponse DeleteFolderMembership =
      DeleteFolderMembershipResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteFolderMembershipResponse'
            Prelude.<$> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFolderMembership where
  hashWithSalt _salt DeleteFolderMembership' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` folderId
      `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` memberType

instance Prelude.NFData DeleteFolderMembership where
  rnf DeleteFolderMembership' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf memberType

instance Data.ToHeaders DeleteFolderMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteFolderMembership where
  toPath DeleteFolderMembership' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/folders/",
        Data.toBS folderId,
        "/members/",
        Data.toBS memberType,
        "/",
        Data.toBS memberId
      ]

instance Data.ToQuery DeleteFolderMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteFolderMembershipResponse' smart constructor.
data DeleteFolderMembershipResponse = DeleteFolderMembershipResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFolderMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'deleteFolderMembershipResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'deleteFolderMembershipResponse_status' - The HTTP status of the request.
--
-- 'httpStatus', 'deleteFolderMembershipResponse_httpStatus' - The response's http status code.
newDeleteFolderMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFolderMembershipResponse
newDeleteFolderMembershipResponse pHttpStatus_ =
  DeleteFolderMembershipResponse'
    { requestId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Web Services request ID for this operation.
deleteFolderMembershipResponse_requestId :: Lens.Lens' DeleteFolderMembershipResponse (Prelude.Maybe Prelude.Text)
deleteFolderMembershipResponse_requestId = Lens.lens (\DeleteFolderMembershipResponse' {requestId} -> requestId) (\s@DeleteFolderMembershipResponse' {} a -> s {requestId = a} :: DeleteFolderMembershipResponse)

-- | The HTTP status of the request.
deleteFolderMembershipResponse_status :: Lens.Lens' DeleteFolderMembershipResponse (Prelude.Maybe Prelude.Int)
deleteFolderMembershipResponse_status = Lens.lens (\DeleteFolderMembershipResponse' {status} -> status) (\s@DeleteFolderMembershipResponse' {} a -> s {status = a} :: DeleteFolderMembershipResponse)

-- | The response's http status code.
deleteFolderMembershipResponse_httpStatus :: Lens.Lens' DeleteFolderMembershipResponse Prelude.Int
deleteFolderMembershipResponse_httpStatus = Lens.lens (\DeleteFolderMembershipResponse' {httpStatus} -> httpStatus) (\s@DeleteFolderMembershipResponse' {} a -> s {httpStatus = a} :: DeleteFolderMembershipResponse)

instance
  Prelude.NFData
    DeleteFolderMembershipResponse
  where
  rnf DeleteFolderMembershipResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
