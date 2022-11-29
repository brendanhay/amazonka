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
-- Module      : Amazonka.GuardDuty.CreateMembers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates member accounts of the current Amazon Web Services account by
-- specifying a list of Amazon Web Services account IDs. This step is a
-- prerequisite for managing the associated member accounts either by
-- invitation or through an organization.
--
-- When using @Create Members@ as an organizations delegated administrator
-- this action will enable GuardDuty in the added member accounts, with the
-- exception of the organization delegated administrator account, which
-- must enable GuardDuty prior to being added as a member.
--
-- If you are adding accounts by invitation use this action after GuardDuty
-- has been enabled in potential member accounts and before using
-- <https://docs.aws.amazon.com/guardduty/latest/APIReference/API_InviteMembers.html Invite Members>
-- .
module Amazonka.GuardDuty.CreateMembers
  ( -- * Creating a Request
    CreateMembers (..),
    newCreateMembers,

    -- * Request Lenses
    createMembers_detectorId,
    createMembers_accountDetails,

    -- * Destructuring the Response
    CreateMembersResponse (..),
    newCreateMembersResponse,

    -- * Response Lenses
    createMembersResponse_httpStatus,
    createMembersResponse_unprocessedAccounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMembers' smart constructor.
data CreateMembers = CreateMembers'
  { -- | The unique ID of the detector of the GuardDuty account that you want to
    -- associate member accounts with.
    detectorId :: Prelude.Text,
    -- | A list of account ID and email address pairs of the accounts that you
    -- want to associate with the GuardDuty administrator account.
    accountDetails :: Prelude.NonEmpty AccountDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'createMembers_detectorId' - The unique ID of the detector of the GuardDuty account that you want to
-- associate member accounts with.
--
-- 'accountDetails', 'createMembers_accountDetails' - A list of account ID and email address pairs of the accounts that you
-- want to associate with the GuardDuty administrator account.
newCreateMembers ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'accountDetails'
  Prelude.NonEmpty AccountDetail ->
  CreateMembers
newCreateMembers pDetectorId_ pAccountDetails_ =
  CreateMembers'
    { detectorId = pDetectorId_,
      accountDetails =
        Lens.coerced Lens.# pAccountDetails_
    }

-- | The unique ID of the detector of the GuardDuty account that you want to
-- associate member accounts with.
createMembers_detectorId :: Lens.Lens' CreateMembers Prelude.Text
createMembers_detectorId = Lens.lens (\CreateMembers' {detectorId} -> detectorId) (\s@CreateMembers' {} a -> s {detectorId = a} :: CreateMembers)

-- | A list of account ID and email address pairs of the accounts that you
-- want to associate with the GuardDuty administrator account.
createMembers_accountDetails :: Lens.Lens' CreateMembers (Prelude.NonEmpty AccountDetail)
createMembers_accountDetails = Lens.lens (\CreateMembers' {accountDetails} -> accountDetails) (\s@CreateMembers' {} a -> s {accountDetails = a} :: CreateMembers) Prelude.. Lens.coerced

instance Core.AWSRequest CreateMembers where
  type
    AWSResponse CreateMembers =
      CreateMembersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMembersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "unprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable CreateMembers where
  hashWithSalt _salt CreateMembers' {..} =
    _salt `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` accountDetails

instance Prelude.NFData CreateMembers where
  rnf CreateMembers' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf accountDetails

instance Core.ToHeaders CreateMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateMembers where
  toJSON CreateMembers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("accountDetails" Core..= accountDetails)
          ]
      )

instance Core.ToPath CreateMembers where
  toPath CreateMembers' {..} =
    Prelude.mconcat
      ["/detector/", Core.toBS detectorId, "/member"]

instance Core.ToQuery CreateMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMembersResponse' smart constructor.
data CreateMembersResponse = CreateMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of objects that include the @accountIds@ of the unprocessed
    -- accounts and a result string that explains why each was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createMembersResponse_httpStatus' - The response's http status code.
--
-- 'unprocessedAccounts', 'createMembersResponse_unprocessedAccounts' - A list of objects that include the @accountIds@ of the unprocessed
-- accounts and a result string that explains why each was unprocessed.
newCreateMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMembersResponse
newCreateMembersResponse pHttpStatus_ =
  CreateMembersResponse'
    { httpStatus = pHttpStatus_,
      unprocessedAccounts = Prelude.mempty
    }

-- | The response's http status code.
createMembersResponse_httpStatus :: Lens.Lens' CreateMembersResponse Prelude.Int
createMembersResponse_httpStatus = Lens.lens (\CreateMembersResponse' {httpStatus} -> httpStatus) (\s@CreateMembersResponse' {} a -> s {httpStatus = a} :: CreateMembersResponse)

-- | A list of objects that include the @accountIds@ of the unprocessed
-- accounts and a result string that explains why each was unprocessed.
createMembersResponse_unprocessedAccounts :: Lens.Lens' CreateMembersResponse [UnprocessedAccount]
createMembersResponse_unprocessedAccounts = Lens.lens (\CreateMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@CreateMembersResponse' {} a -> s {unprocessedAccounts = a} :: CreateMembersResponse) Prelude.. Lens.coerced

instance Prelude.NFData CreateMembersResponse where
  rnf CreateMembersResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf unprocessedAccounts
