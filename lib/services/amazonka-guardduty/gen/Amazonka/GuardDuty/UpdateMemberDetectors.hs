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
-- Module      : Amazonka.GuardDuty.UpdateMemberDetectors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Contains information on member accounts to be updated.
module Amazonka.GuardDuty.UpdateMemberDetectors
  ( -- * Creating a Request
    UpdateMemberDetectors (..),
    newUpdateMemberDetectors,

    -- * Request Lenses
    updateMemberDetectors_dataSources,
    updateMemberDetectors_detectorId,
    updateMemberDetectors_accountIds,

    -- * Destructuring the Response
    UpdateMemberDetectorsResponse (..),
    newUpdateMemberDetectorsResponse,

    -- * Response Lenses
    updateMemberDetectorsResponse_httpStatus,
    updateMemberDetectorsResponse_unprocessedAccounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMemberDetectors' smart constructor.
data UpdateMemberDetectors = UpdateMemberDetectors'
  { -- | Describes which data sources will be updated.
    dataSources :: Prelude.Maybe DataSourceConfigurations,
    -- | The detector ID of the administrator account.
    detectorId :: Prelude.Text,
    -- | A list of member account IDs to be updated.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMemberDetectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSources', 'updateMemberDetectors_dataSources' - Describes which data sources will be updated.
--
-- 'detectorId', 'updateMemberDetectors_detectorId' - The detector ID of the administrator account.
--
-- 'accountIds', 'updateMemberDetectors_accountIds' - A list of member account IDs to be updated.
newUpdateMemberDetectors ::
  -- | 'detectorId'
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  UpdateMemberDetectors
newUpdateMemberDetectors pDetectorId_ pAccountIds_ =
  UpdateMemberDetectors'
    { dataSources =
        Prelude.Nothing,
      detectorId = pDetectorId_,
      accountIds = Lens.coerced Lens.# pAccountIds_
    }

-- | Describes which data sources will be updated.
updateMemberDetectors_dataSources :: Lens.Lens' UpdateMemberDetectors (Prelude.Maybe DataSourceConfigurations)
updateMemberDetectors_dataSources = Lens.lens (\UpdateMemberDetectors' {dataSources} -> dataSources) (\s@UpdateMemberDetectors' {} a -> s {dataSources = a} :: UpdateMemberDetectors)

-- | The detector ID of the administrator account.
updateMemberDetectors_detectorId :: Lens.Lens' UpdateMemberDetectors Prelude.Text
updateMemberDetectors_detectorId = Lens.lens (\UpdateMemberDetectors' {detectorId} -> detectorId) (\s@UpdateMemberDetectors' {} a -> s {detectorId = a} :: UpdateMemberDetectors)

-- | A list of member account IDs to be updated.
updateMemberDetectors_accountIds :: Lens.Lens' UpdateMemberDetectors (Prelude.NonEmpty Prelude.Text)
updateMemberDetectors_accountIds = Lens.lens (\UpdateMemberDetectors' {accountIds} -> accountIds) (\s@UpdateMemberDetectors' {} a -> s {accountIds = a} :: UpdateMemberDetectors) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateMemberDetectors where
  type
    AWSResponse UpdateMemberDetectors =
      UpdateMemberDetectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMemberDetectorsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "unprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable UpdateMemberDetectors where
  hashWithSalt _salt UpdateMemberDetectors' {..} =
    _salt
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` accountIds

instance Prelude.NFData UpdateMemberDetectors where
  rnf UpdateMemberDetectors' {..} =
    Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf accountIds

instance Data.ToHeaders UpdateMemberDetectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMemberDetectors where
  toJSON UpdateMemberDetectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataSources" Data..=) Prelude.<$> dataSources,
            Prelude.Just ("accountIds" Data..= accountIds)
          ]
      )

instance Data.ToPath UpdateMemberDetectors where
  toPath UpdateMemberDetectors' {..} =
    Prelude.mconcat
      [ "/detector/",
        Data.toBS detectorId,
        "/member/detector/update"
      ]

instance Data.ToQuery UpdateMemberDetectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMemberDetectorsResponse' smart constructor.
data UpdateMemberDetectorsResponse = UpdateMemberDetectorsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of member account IDs that were unable to be processed along with
    -- an explanation for why they were not processed.
    unprocessedAccounts :: [UnprocessedAccount]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMemberDetectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMemberDetectorsResponse_httpStatus' - The response's http status code.
--
-- 'unprocessedAccounts', 'updateMemberDetectorsResponse_unprocessedAccounts' - A list of member account IDs that were unable to be processed along with
-- an explanation for why they were not processed.
newUpdateMemberDetectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMemberDetectorsResponse
newUpdateMemberDetectorsResponse pHttpStatus_ =
  UpdateMemberDetectorsResponse'
    { httpStatus =
        pHttpStatus_,
      unprocessedAccounts = Prelude.mempty
    }

-- | The response's http status code.
updateMemberDetectorsResponse_httpStatus :: Lens.Lens' UpdateMemberDetectorsResponse Prelude.Int
updateMemberDetectorsResponse_httpStatus = Lens.lens (\UpdateMemberDetectorsResponse' {httpStatus} -> httpStatus) (\s@UpdateMemberDetectorsResponse' {} a -> s {httpStatus = a} :: UpdateMemberDetectorsResponse)

-- | A list of member account IDs that were unable to be processed along with
-- an explanation for why they were not processed.
updateMemberDetectorsResponse_unprocessedAccounts :: Lens.Lens' UpdateMemberDetectorsResponse [UnprocessedAccount]
updateMemberDetectorsResponse_unprocessedAccounts = Lens.lens (\UpdateMemberDetectorsResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@UpdateMemberDetectorsResponse' {} a -> s {unprocessedAccounts = a} :: UpdateMemberDetectorsResponse) Prelude.. Lens.coerced

instance Prelude.NFData UpdateMemberDetectorsResponse where
  rnf UpdateMemberDetectorsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf unprocessedAccounts
