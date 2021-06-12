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
-- Module      : Network.AWS.Redshift.DescribeLoggingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes whether information, such as queries and connection attempts,
-- is being logged for the specified Amazon Redshift cluster.
module Network.AWS.Redshift.DescribeLoggingStatus
  ( -- * Creating a Request
    DescribeLoggingStatus (..),
    newDescribeLoggingStatus,

    -- * Request Lenses
    describeLoggingStatus_clusterIdentifier,

    -- * Destructuring the Response
    LoggingStatus (..),
    newLoggingStatus,

    -- * Response Lenses
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_bucketName,
    loggingStatus_loggingEnabled,
    loggingStatus_lastFailureTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_lastFailureMessage,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeLoggingStatus' smart constructor.
data DescribeLoggingStatus = DescribeLoggingStatus'
  { -- | The identifier of the cluster from which to get the logging status.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLoggingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'describeLoggingStatus_clusterIdentifier' - The identifier of the cluster from which to get the logging status.
--
-- Example: @examplecluster@
newDescribeLoggingStatus ::
  -- | 'clusterIdentifier'
  Core.Text ->
  DescribeLoggingStatus
newDescribeLoggingStatus pClusterIdentifier_ =
  DescribeLoggingStatus'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The identifier of the cluster from which to get the logging status.
--
-- Example: @examplecluster@
describeLoggingStatus_clusterIdentifier :: Lens.Lens' DescribeLoggingStatus Core.Text
describeLoggingStatus_clusterIdentifier = Lens.lens (\DescribeLoggingStatus' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeLoggingStatus' {} a -> s {clusterIdentifier = a} :: DescribeLoggingStatus)

instance Core.AWSRequest DescribeLoggingStatus where
  type
    AWSResponse DescribeLoggingStatus =
      LoggingStatus
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLoggingStatusResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable DescribeLoggingStatus

instance Core.NFData DescribeLoggingStatus

instance Core.ToHeaders DescribeLoggingStatus where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeLoggingStatus where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLoggingStatus where
  toQuery DescribeLoggingStatus' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeLoggingStatus" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]
