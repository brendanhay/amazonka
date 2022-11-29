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
-- Module      : Amazonka.Redshift.DeletePartner
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a partner integration from a cluster. Data can still flow to the
-- cluster until the integration is deleted at the partner\'s website.
module Amazonka.Redshift.DeletePartner
  ( -- * Creating a Request
    DeletePartner (..),
    newDeletePartner,

    -- * Request Lenses
    deletePartner_accountId,
    deletePartner_clusterIdentifier,
    deletePartner_databaseName,
    deletePartner_partnerName,

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
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePartner' smart constructor.
data DeletePartner = DeletePartner'
  { -- | The Amazon Web Services account ID that owns the cluster.
    accountId :: Prelude.Text,
    -- | The cluster identifier of the cluster that receives data from the
    -- partner.
    clusterIdentifier :: Prelude.Text,
    -- | The name of the database that receives data from the partner.
    databaseName :: Prelude.Text,
    -- | The name of the partner that is authorized to send data.
    partnerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePartner' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deletePartner_accountId' - The Amazon Web Services account ID that owns the cluster.
--
-- 'clusterIdentifier', 'deletePartner_clusterIdentifier' - The cluster identifier of the cluster that receives data from the
-- partner.
--
-- 'databaseName', 'deletePartner_databaseName' - The name of the database that receives data from the partner.
--
-- 'partnerName', 'deletePartner_partnerName' - The name of the partner that is authorized to send data.
newDeletePartner ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'clusterIdentifier'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'partnerName'
  Prelude.Text ->
  DeletePartner
newDeletePartner
  pAccountId_
  pClusterIdentifier_
  pDatabaseName_
  pPartnerName_ =
    DeletePartner'
      { accountId = pAccountId_,
        clusterIdentifier = pClusterIdentifier_,
        databaseName = pDatabaseName_,
        partnerName = pPartnerName_
      }

-- | The Amazon Web Services account ID that owns the cluster.
deletePartner_accountId :: Lens.Lens' DeletePartner Prelude.Text
deletePartner_accountId = Lens.lens (\DeletePartner' {accountId} -> accountId) (\s@DeletePartner' {} a -> s {accountId = a} :: DeletePartner)

-- | The cluster identifier of the cluster that receives data from the
-- partner.
deletePartner_clusterIdentifier :: Lens.Lens' DeletePartner Prelude.Text
deletePartner_clusterIdentifier = Lens.lens (\DeletePartner' {clusterIdentifier} -> clusterIdentifier) (\s@DeletePartner' {} a -> s {clusterIdentifier = a} :: DeletePartner)

-- | The name of the database that receives data from the partner.
deletePartner_databaseName :: Lens.Lens' DeletePartner Prelude.Text
deletePartner_databaseName = Lens.lens (\DeletePartner' {databaseName} -> databaseName) (\s@DeletePartner' {} a -> s {databaseName = a} :: DeletePartner)

-- | The name of the partner that is authorized to send data.
deletePartner_partnerName :: Lens.Lens' DeletePartner Prelude.Text
deletePartner_partnerName = Lens.lens (\DeletePartner' {partnerName} -> partnerName) (\s@DeletePartner' {} a -> s {partnerName = a} :: DeletePartner)

instance Core.AWSRequest DeletePartner where
  type
    AWSResponse DeletePartner =
      PartnerIntegrationOutputMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeletePartnerResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable DeletePartner where
  hashWithSalt _salt DeletePartner' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` partnerName

instance Prelude.NFData DeletePartner where
  rnf DeletePartner' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf partnerName

instance Core.ToHeaders DeletePartner where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeletePartner where
  toPath = Prelude.const "/"

instance Core.ToQuery DeletePartner where
  toQuery DeletePartner' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeletePartner" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "AccountId" Core.=: accountId,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "DatabaseName" Core.=: databaseName,
        "PartnerName" Core.=: partnerName
      ]
