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
-- Module      : Amazonka.Redshift.DescribePartners
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the partner integrations defined for a
-- cluster.
module Amazonka.Redshift.DescribePartners
  ( -- * Creating a Request
    DescribePartners (..),
    newDescribePartners,

    -- * Request Lenses
    describePartners_databaseName,
    describePartners_partnerName,
    describePartners_accountId,
    describePartners_clusterIdentifier,

    -- * Destructuring the Response
    DescribePartnersResponse (..),
    newDescribePartnersResponse,

    -- * Response Lenses
    describePartnersResponse_partnerIntegrationInfoList,
    describePartnersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePartners' smart constructor.
data DescribePartners = DescribePartners'
  { -- | The name of the database whose partner integration is being described.
    -- If database name is not specified, then all databases in the cluster are
    -- described.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the partner that is being described. If partner name is not
    -- specified, then all partner integrations are described.
    partnerName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID that owns the cluster.
    accountId :: Prelude.Text,
    -- | The cluster identifier of the cluster whose partner integration is being
    -- described.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePartners' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseName', 'describePartners_databaseName' - The name of the database whose partner integration is being described.
-- If database name is not specified, then all databases in the cluster are
-- described.
--
-- 'partnerName', 'describePartners_partnerName' - The name of the partner that is being described. If partner name is not
-- specified, then all partner integrations are described.
--
-- 'accountId', 'describePartners_accountId' - The Amazon Web Services account ID that owns the cluster.
--
-- 'clusterIdentifier', 'describePartners_clusterIdentifier' - The cluster identifier of the cluster whose partner integration is being
-- described.
newDescribePartners ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'clusterIdentifier'
  Prelude.Text ->
  DescribePartners
newDescribePartners pAccountId_ pClusterIdentifier_ =
  DescribePartners'
    { databaseName = Prelude.Nothing,
      partnerName = Prelude.Nothing,
      accountId = pAccountId_,
      clusterIdentifier = pClusterIdentifier_
    }

-- | The name of the database whose partner integration is being described.
-- If database name is not specified, then all databases in the cluster are
-- described.
describePartners_databaseName :: Lens.Lens' DescribePartners (Prelude.Maybe Prelude.Text)
describePartners_databaseName = Lens.lens (\DescribePartners' {databaseName} -> databaseName) (\s@DescribePartners' {} a -> s {databaseName = a} :: DescribePartners)

-- | The name of the partner that is being described. If partner name is not
-- specified, then all partner integrations are described.
describePartners_partnerName :: Lens.Lens' DescribePartners (Prelude.Maybe Prelude.Text)
describePartners_partnerName = Lens.lens (\DescribePartners' {partnerName} -> partnerName) (\s@DescribePartners' {} a -> s {partnerName = a} :: DescribePartners)

-- | The Amazon Web Services account ID that owns the cluster.
describePartners_accountId :: Lens.Lens' DescribePartners Prelude.Text
describePartners_accountId = Lens.lens (\DescribePartners' {accountId} -> accountId) (\s@DescribePartners' {} a -> s {accountId = a} :: DescribePartners)

-- | The cluster identifier of the cluster whose partner integration is being
-- described.
describePartners_clusterIdentifier :: Lens.Lens' DescribePartners Prelude.Text
describePartners_clusterIdentifier = Lens.lens (\DescribePartners' {clusterIdentifier} -> clusterIdentifier) (\s@DescribePartners' {} a -> s {clusterIdentifier = a} :: DescribePartners)

instance Core.AWSRequest DescribePartners where
  type
    AWSResponse DescribePartners =
      DescribePartnersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribePartnersResult"
      ( \s h x ->
          DescribePartnersResponse'
            Prelude.<$> ( x Data..@? "PartnerIntegrationInfoList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may
                              (Data.parseXMLList "PartnerIntegrationInfo")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePartners where
  hashWithSalt _salt DescribePartners' {..} =
    _salt `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` partnerName
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData DescribePartners where
  rnf DescribePartners' {..} =
    Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf partnerName
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf clusterIdentifier

instance Data.ToHeaders DescribePartners where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribePartners where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePartners where
  toQuery DescribePartners' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribePartners" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "DatabaseName" Data.=: databaseName,
        "PartnerName" Data.=: partnerName,
        "AccountId" Data.=: accountId,
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]

-- | /See:/ 'newDescribePartnersResponse' smart constructor.
data DescribePartnersResponse = DescribePartnersResponse'
  { -- | A list of partner integrations.
    partnerIntegrationInfoList :: Prelude.Maybe [PartnerIntegrationInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePartnersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partnerIntegrationInfoList', 'describePartnersResponse_partnerIntegrationInfoList' - A list of partner integrations.
--
-- 'httpStatus', 'describePartnersResponse_httpStatus' - The response's http status code.
newDescribePartnersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePartnersResponse
newDescribePartnersResponse pHttpStatus_ =
  DescribePartnersResponse'
    { partnerIntegrationInfoList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of partner integrations.
describePartnersResponse_partnerIntegrationInfoList :: Lens.Lens' DescribePartnersResponse (Prelude.Maybe [PartnerIntegrationInfo])
describePartnersResponse_partnerIntegrationInfoList = Lens.lens (\DescribePartnersResponse' {partnerIntegrationInfoList} -> partnerIntegrationInfoList) (\s@DescribePartnersResponse' {} a -> s {partnerIntegrationInfoList = a} :: DescribePartnersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describePartnersResponse_httpStatus :: Lens.Lens' DescribePartnersResponse Prelude.Int
describePartnersResponse_httpStatus = Lens.lens (\DescribePartnersResponse' {httpStatus} -> httpStatus) (\s@DescribePartnersResponse' {} a -> s {httpStatus = a} :: DescribePartnersResponse)

instance Prelude.NFData DescribePartnersResponse where
  rnf DescribePartnersResponse' {..} =
    Prelude.rnf partnerIntegrationInfoList
      `Prelude.seq` Prelude.rnf httpStatus
