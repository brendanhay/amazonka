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
-- Module      : Network.AWS.GuardDuty.GetMembers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves GuardDuty member accounts (of the current GuardDuty
-- administrator account) specified by the account IDs.
module Network.AWS.GuardDuty.GetMembers
  ( -- * Creating a Request
    GetMembers (..),
    newGetMembers,

    -- * Request Lenses
    getMembers_detectorId,
    getMembers_accountIds,

    -- * Destructuring the Response
    GetMembersResponse (..),
    newGetMembersResponse,

    -- * Response Lenses
    getMembersResponse_httpStatus,
    getMembersResponse_members,
    getMembersResponse_unprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMembers' smart constructor.
data GetMembers = GetMembers'
  { -- | The unique ID of the detector of the GuardDuty account whose members you
    -- want to retrieve.
    detectorId :: Prelude.Text,
    -- | A list of account IDs of the GuardDuty member accounts that you want to
    -- describe.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMembers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'getMembers_detectorId' - The unique ID of the detector of the GuardDuty account whose members you
-- want to retrieve.
--
-- 'accountIds', 'getMembers_accountIds' - A list of account IDs of the GuardDuty member accounts that you want to
-- describe.
newGetMembers ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  GetMembers
newGetMembers pDetectorId_ pAccountIds_ =
  GetMembers'
    { detectorId = pDetectorId_,
      accountIds = Prelude._Coerce Lens.# pAccountIds_
    }

-- | The unique ID of the detector of the GuardDuty account whose members you
-- want to retrieve.
getMembers_detectorId :: Lens.Lens' GetMembers Prelude.Text
getMembers_detectorId = Lens.lens (\GetMembers' {detectorId} -> detectorId) (\s@GetMembers' {} a -> s {detectorId = a} :: GetMembers)

-- | A list of account IDs of the GuardDuty member accounts that you want to
-- describe.
getMembers_accountIds :: Lens.Lens' GetMembers (Prelude.NonEmpty Prelude.Text)
getMembers_accountIds = Lens.lens (\GetMembers' {accountIds} -> accountIds) (\s@GetMembers' {} a -> s {accountIds = a} :: GetMembers) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest GetMembers where
  type Rs GetMembers = GetMembersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMembersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..?> "members" Prelude..!@ Prelude.mempty)
            Prelude.<*> ( x Prelude..?> "unprocessedAccounts"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetMembers

instance Prelude.NFData GetMembers

instance Prelude.ToHeaders GetMembers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetMembers where
  toJSON GetMembers' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountIds" Prelude..= accountIds)]
      )

instance Prelude.ToPath GetMembers where
  toPath GetMembers' {..} =
    Prelude.mconcat
      [ "/detector/",
        Prelude.toBS detectorId,
        "/member/get"
      ]

instance Prelude.ToQuery GetMembers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMembersResponse' smart constructor.
data GetMembersResponse = GetMembersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of members.
    members :: [Member],
    -- | A list of objects that contain the unprocessed account and a result
    -- string that explains why it was unprocessed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetMembersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getMembersResponse_httpStatus' - The response's http status code.
--
-- 'members', 'getMembersResponse_members' - A list of members.
--
-- 'unprocessedAccounts', 'getMembersResponse_unprocessedAccounts' - A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
newGetMembersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMembersResponse
newGetMembersResponse pHttpStatus_ =
  GetMembersResponse'
    { httpStatus = pHttpStatus_,
      members = Prelude.mempty,
      unprocessedAccounts = Prelude.mempty
    }

-- | The response's http status code.
getMembersResponse_httpStatus :: Lens.Lens' GetMembersResponse Prelude.Int
getMembersResponse_httpStatus = Lens.lens (\GetMembersResponse' {httpStatus} -> httpStatus) (\s@GetMembersResponse' {} a -> s {httpStatus = a} :: GetMembersResponse)

-- | A list of members.
getMembersResponse_members :: Lens.Lens' GetMembersResponse [Member]
getMembersResponse_members = Lens.lens (\GetMembersResponse' {members} -> members) (\s@GetMembersResponse' {} a -> s {members = a} :: GetMembersResponse) Prelude.. Prelude._Coerce

-- | A list of objects that contain the unprocessed account and a result
-- string that explains why it was unprocessed.
getMembersResponse_unprocessedAccounts :: Lens.Lens' GetMembersResponse [UnprocessedAccount]
getMembersResponse_unprocessedAccounts = Lens.lens (\GetMembersResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@GetMembersResponse' {} a -> s {unprocessedAccounts = a} :: GetMembersResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData GetMembersResponse
