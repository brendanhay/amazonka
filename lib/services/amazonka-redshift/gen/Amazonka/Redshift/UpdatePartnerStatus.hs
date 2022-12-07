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
-- Module      : Amazonka.Redshift.UpdatePartnerStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of a partner integration.
module Amazonka.Redshift.UpdatePartnerStatus
  ( -- * Creating a Request
    UpdatePartnerStatus (..),
    newUpdatePartnerStatus,

    -- * Request Lenses
    updatePartnerStatus_statusMessage,
    updatePartnerStatus_accountId,
    updatePartnerStatus_clusterIdentifier,
    updatePartnerStatus_databaseName,
    updatePartnerStatus_partnerName,
    updatePartnerStatus_status,

    -- * Destructuring the Response
    PartnerIntegrationOutputMessage (..),
    newPartnerIntegrationOutputMessage,

    -- * Response Lenses
    partnerIntegrationOutputMessage_databaseName,
    partnerIntegrationOutputMessage_partnerName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePartnerStatus' smart constructor.
data UpdatePartnerStatus = UpdatePartnerStatus'
  { -- | The status message provided by the partner.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID that owns the cluster.
    accountId :: Prelude.Text,
    -- | The cluster identifier of the cluster whose partner integration status
    -- is being updated.
    clusterIdentifier :: Prelude.Text,
    -- | The name of the database whose partner integration status is being
    -- updated.
    databaseName :: Prelude.Text,
    -- | The name of the partner whose integration status is being updated.
    partnerName :: Prelude.Text,
    -- | The value of the updated status.
    status :: PartnerIntegrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePartnerStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'updatePartnerStatus_statusMessage' - The status message provided by the partner.
--
-- 'accountId', 'updatePartnerStatus_accountId' - The Amazon Web Services account ID that owns the cluster.
--
-- 'clusterIdentifier', 'updatePartnerStatus_clusterIdentifier' - The cluster identifier of the cluster whose partner integration status
-- is being updated.
--
-- 'databaseName', 'updatePartnerStatus_databaseName' - The name of the database whose partner integration status is being
-- updated.
--
-- 'partnerName', 'updatePartnerStatus_partnerName' - The name of the partner whose integration status is being updated.
--
-- 'status', 'updatePartnerStatus_status' - The value of the updated status.
newUpdatePartnerStatus ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'clusterIdentifier'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'partnerName'
  Prelude.Text ->
  -- | 'status'
  PartnerIntegrationStatus ->
  UpdatePartnerStatus
newUpdatePartnerStatus
  pAccountId_
  pClusterIdentifier_
  pDatabaseName_
  pPartnerName_
  pStatus_ =
    UpdatePartnerStatus'
      { statusMessage =
          Prelude.Nothing,
        accountId = pAccountId_,
        clusterIdentifier = pClusterIdentifier_,
        databaseName = pDatabaseName_,
        partnerName = pPartnerName_,
        status = pStatus_
      }

-- | The status message provided by the partner.
updatePartnerStatus_statusMessage :: Lens.Lens' UpdatePartnerStatus (Prelude.Maybe Prelude.Text)
updatePartnerStatus_statusMessage = Lens.lens (\UpdatePartnerStatus' {statusMessage} -> statusMessage) (\s@UpdatePartnerStatus' {} a -> s {statusMessage = a} :: UpdatePartnerStatus)

-- | The Amazon Web Services account ID that owns the cluster.
updatePartnerStatus_accountId :: Lens.Lens' UpdatePartnerStatus Prelude.Text
updatePartnerStatus_accountId = Lens.lens (\UpdatePartnerStatus' {accountId} -> accountId) (\s@UpdatePartnerStatus' {} a -> s {accountId = a} :: UpdatePartnerStatus)

-- | The cluster identifier of the cluster whose partner integration status
-- is being updated.
updatePartnerStatus_clusterIdentifier :: Lens.Lens' UpdatePartnerStatus Prelude.Text
updatePartnerStatus_clusterIdentifier = Lens.lens (\UpdatePartnerStatus' {clusterIdentifier} -> clusterIdentifier) (\s@UpdatePartnerStatus' {} a -> s {clusterIdentifier = a} :: UpdatePartnerStatus)

-- | The name of the database whose partner integration status is being
-- updated.
updatePartnerStatus_databaseName :: Lens.Lens' UpdatePartnerStatus Prelude.Text
updatePartnerStatus_databaseName = Lens.lens (\UpdatePartnerStatus' {databaseName} -> databaseName) (\s@UpdatePartnerStatus' {} a -> s {databaseName = a} :: UpdatePartnerStatus)

-- | The name of the partner whose integration status is being updated.
updatePartnerStatus_partnerName :: Lens.Lens' UpdatePartnerStatus Prelude.Text
updatePartnerStatus_partnerName = Lens.lens (\UpdatePartnerStatus' {partnerName} -> partnerName) (\s@UpdatePartnerStatus' {} a -> s {partnerName = a} :: UpdatePartnerStatus)

-- | The value of the updated status.
updatePartnerStatus_status :: Lens.Lens' UpdatePartnerStatus PartnerIntegrationStatus
updatePartnerStatus_status = Lens.lens (\UpdatePartnerStatus' {status} -> status) (\s@UpdatePartnerStatus' {} a -> s {status = a} :: UpdatePartnerStatus)

instance Core.AWSRequest UpdatePartnerStatus where
  type
    AWSResponse UpdatePartnerStatus =
      PartnerIntegrationOutputMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UpdatePartnerStatusResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable UpdatePartnerStatus where
  hashWithSalt _salt UpdatePartnerStatus' {..} =
    _salt `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` partnerName
      `Prelude.hashWithSalt` status

instance Prelude.NFData UpdatePartnerStatus where
  rnf UpdatePartnerStatus' {..} =
    Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf partnerName
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders UpdatePartnerStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdatePartnerStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdatePartnerStatus where
  toQuery UpdatePartnerStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UpdatePartnerStatus" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "StatusMessage" Data.=: statusMessage,
        "AccountId" Data.=: accountId,
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "DatabaseName" Data.=: databaseName,
        "PartnerName" Data.=: partnerName,
        "Status" Data.=: status
      ]
