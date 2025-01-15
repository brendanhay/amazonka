{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Redshift.Types.PartnerIntegrationInputMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.PartnerIntegrationInputMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | /See:/ 'newPartnerIntegrationInputMessage' smart constructor.
data PartnerIntegrationInputMessage = PartnerIntegrationInputMessage'
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
-- Create a value of 'PartnerIntegrationInputMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'partnerIntegrationInputMessage_accountId' - The Amazon Web Services account ID that owns the cluster.
--
-- 'clusterIdentifier', 'partnerIntegrationInputMessage_clusterIdentifier' - The cluster identifier of the cluster that receives data from the
-- partner.
--
-- 'databaseName', 'partnerIntegrationInputMessage_databaseName' - The name of the database that receives data from the partner.
--
-- 'partnerName', 'partnerIntegrationInputMessage_partnerName' - The name of the partner that is authorized to send data.
newPartnerIntegrationInputMessage ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'clusterIdentifier'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'partnerName'
  Prelude.Text ->
  PartnerIntegrationInputMessage
newPartnerIntegrationInputMessage
  pAccountId_
  pClusterIdentifier_
  pDatabaseName_
  pPartnerName_ =
    PartnerIntegrationInputMessage'
      { accountId =
          pAccountId_,
        clusterIdentifier = pClusterIdentifier_,
        databaseName = pDatabaseName_,
        partnerName = pPartnerName_
      }

-- | The Amazon Web Services account ID that owns the cluster.
partnerIntegrationInputMessage_accountId :: Lens.Lens' PartnerIntegrationInputMessage Prelude.Text
partnerIntegrationInputMessage_accountId = Lens.lens (\PartnerIntegrationInputMessage' {accountId} -> accountId) (\s@PartnerIntegrationInputMessage' {} a -> s {accountId = a} :: PartnerIntegrationInputMessage)

-- | The cluster identifier of the cluster that receives data from the
-- partner.
partnerIntegrationInputMessage_clusterIdentifier :: Lens.Lens' PartnerIntegrationInputMessage Prelude.Text
partnerIntegrationInputMessage_clusterIdentifier = Lens.lens (\PartnerIntegrationInputMessage' {clusterIdentifier} -> clusterIdentifier) (\s@PartnerIntegrationInputMessage' {} a -> s {clusterIdentifier = a} :: PartnerIntegrationInputMessage)

-- | The name of the database that receives data from the partner.
partnerIntegrationInputMessage_databaseName :: Lens.Lens' PartnerIntegrationInputMessage Prelude.Text
partnerIntegrationInputMessage_databaseName = Lens.lens (\PartnerIntegrationInputMessage' {databaseName} -> databaseName) (\s@PartnerIntegrationInputMessage' {} a -> s {databaseName = a} :: PartnerIntegrationInputMessage)

-- | The name of the partner that is authorized to send data.
partnerIntegrationInputMessage_partnerName :: Lens.Lens' PartnerIntegrationInputMessage Prelude.Text
partnerIntegrationInputMessage_partnerName = Lens.lens (\PartnerIntegrationInputMessage' {partnerName} -> partnerName) (\s@PartnerIntegrationInputMessage' {} a -> s {partnerName = a} :: PartnerIntegrationInputMessage)

instance
  Prelude.Hashable
    PartnerIntegrationInputMessage
  where
  hashWithSalt
    _salt
    PartnerIntegrationInputMessage' {..} =
      _salt
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` clusterIdentifier
        `Prelude.hashWithSalt` databaseName
        `Prelude.hashWithSalt` partnerName

instance
  Prelude.NFData
    PartnerIntegrationInputMessage
  where
  rnf PartnerIntegrationInputMessage' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf clusterIdentifier `Prelude.seq`
        Prelude.rnf databaseName `Prelude.seq`
          Prelude.rnf partnerName

instance Data.ToQuery PartnerIntegrationInputMessage where
  toQuery PartnerIntegrationInputMessage' {..} =
    Prelude.mconcat
      [ "AccountId" Data.=: accountId,
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "DatabaseName" Data.=: databaseName,
        "PartnerName" Data.=: partnerName
      ]
