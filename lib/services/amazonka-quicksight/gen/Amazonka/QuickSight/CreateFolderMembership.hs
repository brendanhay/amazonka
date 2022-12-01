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
-- Module      : Amazonka.QuickSight.CreateFolderMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an asset, such as a dashboard, analysis, or dataset into a folder.
module Amazonka.QuickSight.CreateFolderMembership
  ( -- * Creating a Request
    CreateFolderMembership (..),
    newCreateFolderMembership,

    -- * Request Lenses
    createFolderMembership_awsAccountId,
    createFolderMembership_folderId,
    createFolderMembership_memberId,
    createFolderMembership_memberType,

    -- * Destructuring the Response
    CreateFolderMembershipResponse (..),
    newCreateFolderMembershipResponse,

    -- * Response Lenses
    createFolderMembershipResponse_requestId,
    createFolderMembershipResponse_status,
    createFolderMembershipResponse_folderMember,
    createFolderMembershipResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFolderMembership' smart constructor.
data CreateFolderMembership = CreateFolderMembership'
  { -- | The ID for the Amazon Web Services account that contains the folder.
    awsAccountId :: Prelude.Text,
    -- | The ID of the folder.
    folderId :: Prelude.Text,
    -- | The ID of the asset (the dashboard, analysis, or dataset).
    memberId :: Prelude.Text,
    -- | The type of the member, including @DASHBOARD@, @ANALYSIS@, and
    -- @DATASET@.
    memberType :: MemberType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFolderMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'createFolderMembership_awsAccountId' - The ID for the Amazon Web Services account that contains the folder.
--
-- 'folderId', 'createFolderMembership_folderId' - The ID of the folder.
--
-- 'memberId', 'createFolderMembership_memberId' - The ID of the asset (the dashboard, analysis, or dataset).
--
-- 'memberType', 'createFolderMembership_memberType' - The type of the member, including @DASHBOARD@, @ANALYSIS@, and
-- @DATASET@.
newCreateFolderMembership ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  -- | 'memberId'
  Prelude.Text ->
  -- | 'memberType'
  MemberType ->
  CreateFolderMembership
newCreateFolderMembership
  pAwsAccountId_
  pFolderId_
  pMemberId_
  pMemberType_ =
    CreateFolderMembership'
      { awsAccountId =
          pAwsAccountId_,
        folderId = pFolderId_,
        memberId = pMemberId_,
        memberType = pMemberType_
      }

-- | The ID for the Amazon Web Services account that contains the folder.
createFolderMembership_awsAccountId :: Lens.Lens' CreateFolderMembership Prelude.Text
createFolderMembership_awsAccountId = Lens.lens (\CreateFolderMembership' {awsAccountId} -> awsAccountId) (\s@CreateFolderMembership' {} a -> s {awsAccountId = a} :: CreateFolderMembership)

-- | The ID of the folder.
createFolderMembership_folderId :: Lens.Lens' CreateFolderMembership Prelude.Text
createFolderMembership_folderId = Lens.lens (\CreateFolderMembership' {folderId} -> folderId) (\s@CreateFolderMembership' {} a -> s {folderId = a} :: CreateFolderMembership)

-- | The ID of the asset (the dashboard, analysis, or dataset).
createFolderMembership_memberId :: Lens.Lens' CreateFolderMembership Prelude.Text
createFolderMembership_memberId = Lens.lens (\CreateFolderMembership' {memberId} -> memberId) (\s@CreateFolderMembership' {} a -> s {memberId = a} :: CreateFolderMembership)

-- | The type of the member, including @DASHBOARD@, @ANALYSIS@, and
-- @DATASET@.
createFolderMembership_memberType :: Lens.Lens' CreateFolderMembership MemberType
createFolderMembership_memberType = Lens.lens (\CreateFolderMembership' {memberType} -> memberType) (\s@CreateFolderMembership' {} a -> s {memberType = a} :: CreateFolderMembership)

instance Core.AWSRequest CreateFolderMembership where
  type
    AWSResponse CreateFolderMembership =
      CreateFolderMembershipResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFolderMembershipResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "FolderMember")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFolderMembership where
  hashWithSalt _salt CreateFolderMembership' {..} =
    _salt `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` folderId
      `Prelude.hashWithSalt` memberId
      `Prelude.hashWithSalt` memberType

instance Prelude.NFData CreateFolderMembership where
  rnf CreateFolderMembership' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId
      `Prelude.seq` Prelude.rnf memberId
      `Prelude.seq` Prelude.rnf memberType

instance Core.ToHeaders CreateFolderMembership where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFolderMembership where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CreateFolderMembership where
  toPath CreateFolderMembership' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/folders/",
        Core.toBS folderId,
        "/members/",
        Core.toBS memberType,
        "/",
        Core.toBS memberId
      ]

instance Core.ToQuery CreateFolderMembership where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFolderMembershipResponse' smart constructor.
data CreateFolderMembershipResponse = CreateFolderMembershipResponse'
  { -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Maybe Prelude.Int,
    -- | Information about the member in the folder.
    folderMember :: Prelude.Maybe FolderMember,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFolderMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'createFolderMembershipResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'status', 'createFolderMembershipResponse_status' - The HTTP status of the request.
--
-- 'folderMember', 'createFolderMembershipResponse_folderMember' - Information about the member in the folder.
--
-- 'httpStatus', 'createFolderMembershipResponse_httpStatus' - The response's http status code.
newCreateFolderMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFolderMembershipResponse
newCreateFolderMembershipResponse pHttpStatus_ =
  CreateFolderMembershipResponse'
    { requestId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      folderMember = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Web Services request ID for this operation.
createFolderMembershipResponse_requestId :: Lens.Lens' CreateFolderMembershipResponse (Prelude.Maybe Prelude.Text)
createFolderMembershipResponse_requestId = Lens.lens (\CreateFolderMembershipResponse' {requestId} -> requestId) (\s@CreateFolderMembershipResponse' {} a -> s {requestId = a} :: CreateFolderMembershipResponse)

-- | The HTTP status of the request.
createFolderMembershipResponse_status :: Lens.Lens' CreateFolderMembershipResponse (Prelude.Maybe Prelude.Int)
createFolderMembershipResponse_status = Lens.lens (\CreateFolderMembershipResponse' {status} -> status) (\s@CreateFolderMembershipResponse' {} a -> s {status = a} :: CreateFolderMembershipResponse)

-- | Information about the member in the folder.
createFolderMembershipResponse_folderMember :: Lens.Lens' CreateFolderMembershipResponse (Prelude.Maybe FolderMember)
createFolderMembershipResponse_folderMember = Lens.lens (\CreateFolderMembershipResponse' {folderMember} -> folderMember) (\s@CreateFolderMembershipResponse' {} a -> s {folderMember = a} :: CreateFolderMembershipResponse)

-- | The response's http status code.
createFolderMembershipResponse_httpStatus :: Lens.Lens' CreateFolderMembershipResponse Prelude.Int
createFolderMembershipResponse_httpStatus = Lens.lens (\CreateFolderMembershipResponse' {httpStatus} -> httpStatus) (\s@CreateFolderMembershipResponse' {} a -> s {httpStatus = a} :: CreateFolderMembershipResponse)

instance
  Prelude.NFData
    CreateFolderMembershipResponse
  where
  rnf CreateFolderMembershipResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf folderMember
      `Prelude.seq` Prelude.rnf httpStatus
