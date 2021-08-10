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
-- Module      : Network.AWS.GuardDuty.GetMemberDetectors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes which data sources are enabled for the member account\'s
-- detector.
module Network.AWS.GuardDuty.GetMemberDetectors
  ( -- * Creating a Request
    GetMemberDetectors (..),
    newGetMemberDetectors,

    -- * Request Lenses
    getMemberDetectors_detectorId,
    getMemberDetectors_accountIds,

    -- * Destructuring the Response
    GetMemberDetectorsResponse (..),
    newGetMemberDetectorsResponse,

    -- * Response Lenses
    getMemberDetectorsResponse_httpStatus,
    getMemberDetectorsResponse_memberDataSourceConfigurations,
    getMemberDetectorsResponse_unprocessedAccounts,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMemberDetectors' smart constructor.
data GetMemberDetectors = GetMemberDetectors'
  { -- | The detector ID for the administrator account.
    detectorId :: Prelude.Text,
    -- | The account ID of the member account.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMemberDetectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'getMemberDetectors_detectorId' - The detector ID for the administrator account.
--
-- 'accountIds', 'getMemberDetectors_accountIds' - The account ID of the member account.
newGetMemberDetectors ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  GetMemberDetectors
newGetMemberDetectors pDetectorId_ pAccountIds_ =
  GetMemberDetectors'
    { detectorId = pDetectorId_,
      accountIds = Lens._Coerce Lens.# pAccountIds_
    }

-- | The detector ID for the administrator account.
getMemberDetectors_detectorId :: Lens.Lens' GetMemberDetectors Prelude.Text
getMemberDetectors_detectorId = Lens.lens (\GetMemberDetectors' {detectorId} -> detectorId) (\s@GetMemberDetectors' {} a -> s {detectorId = a} :: GetMemberDetectors)

-- | The account ID of the member account.
getMemberDetectors_accountIds :: Lens.Lens' GetMemberDetectors (Prelude.NonEmpty Prelude.Text)
getMemberDetectors_accountIds = Lens.lens (\GetMemberDetectors' {accountIds} -> accountIds) (\s@GetMemberDetectors' {} a -> s {accountIds = a} :: GetMemberDetectors) Prelude.. Lens._Coerce

instance Core.AWSRequest GetMemberDetectors where
  type
    AWSResponse GetMemberDetectors =
      GetMemberDetectorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMemberDetectorsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "members")
            Prelude.<*> ( x Core..?> "unprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetMemberDetectors

instance Prelude.NFData GetMemberDetectors

instance Core.ToHeaders GetMemberDetectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMemberDetectors where
  toJSON GetMemberDetectors' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("accountIds" Core..= accountIds)]
      )

instance Core.ToPath GetMemberDetectors where
  toPath GetMemberDetectors' {..} =
    Prelude.mconcat
      [ "/detector/",
        Core.toBS detectorId,
        "/member/detector/get"
      ]

instance Core.ToQuery GetMemberDetectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMemberDetectorsResponse' smart constructor.
data GetMemberDetectorsResponse = GetMemberDetectorsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object that describes which data sources are enabled for a member
    -- account.
    memberDataSourceConfigurations :: Prelude.NonEmpty MemberDataSourceConfiguration,
    -- | A list of member account IDs that were unable to be processed along with
    -- an explanation for why they were not processed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMemberDetectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getMemberDetectorsResponse_httpStatus' - The response's http status code.
--
-- 'memberDataSourceConfigurations', 'getMemberDetectorsResponse_memberDataSourceConfigurations' - An object that describes which data sources are enabled for a member
-- account.
--
-- 'unprocessedAccounts', 'getMemberDetectorsResponse_unprocessedAccounts' - A list of member account IDs that were unable to be processed along with
-- an explanation for why they were not processed.
newGetMemberDetectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'memberDataSourceConfigurations'
  Prelude.NonEmpty MemberDataSourceConfiguration ->
  GetMemberDetectorsResponse
newGetMemberDetectorsResponse
  pHttpStatus_
  pMemberDataSourceConfigurations_ =
    GetMemberDetectorsResponse'
      { httpStatus =
          pHttpStatus_,
        memberDataSourceConfigurations =
          Lens._Coerce
            Lens.# pMemberDataSourceConfigurations_,
        unprocessedAccounts = Prelude.mempty
      }

-- | The response's http status code.
getMemberDetectorsResponse_httpStatus :: Lens.Lens' GetMemberDetectorsResponse Prelude.Int
getMemberDetectorsResponse_httpStatus = Lens.lens (\GetMemberDetectorsResponse' {httpStatus} -> httpStatus) (\s@GetMemberDetectorsResponse' {} a -> s {httpStatus = a} :: GetMemberDetectorsResponse)

-- | An object that describes which data sources are enabled for a member
-- account.
getMemberDetectorsResponse_memberDataSourceConfigurations :: Lens.Lens' GetMemberDetectorsResponse (Prelude.NonEmpty MemberDataSourceConfiguration)
getMemberDetectorsResponse_memberDataSourceConfigurations = Lens.lens (\GetMemberDetectorsResponse' {memberDataSourceConfigurations} -> memberDataSourceConfigurations) (\s@GetMemberDetectorsResponse' {} a -> s {memberDataSourceConfigurations = a} :: GetMemberDetectorsResponse) Prelude.. Lens._Coerce

-- | A list of member account IDs that were unable to be processed along with
-- an explanation for why they were not processed.
getMemberDetectorsResponse_unprocessedAccounts :: Lens.Lens' GetMemberDetectorsResponse [UnprocessedAccount]
getMemberDetectorsResponse_unprocessedAccounts = Lens.lens (\GetMemberDetectorsResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@GetMemberDetectorsResponse' {} a -> s {unprocessedAccounts = a} :: GetMemberDetectorsResponse) Prelude.. Lens._Coerce

instance Prelude.NFData GetMemberDetectorsResponse
