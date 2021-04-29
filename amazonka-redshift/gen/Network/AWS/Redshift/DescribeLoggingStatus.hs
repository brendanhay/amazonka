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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeLoggingStatus
newDescribeLoggingStatus pClusterIdentifier_ =
  DescribeLoggingStatus'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The identifier of the cluster from which to get the logging status.
--
-- Example: @examplecluster@
describeLoggingStatus_clusterIdentifier :: Lens.Lens' DescribeLoggingStatus Prelude.Text
describeLoggingStatus_clusterIdentifier = Lens.lens (\DescribeLoggingStatus' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeLoggingStatus' {} a -> s {clusterIdentifier = a} :: DescribeLoggingStatus)

instance Prelude.AWSRequest DescribeLoggingStatus where
  type Rs DescribeLoggingStatus = LoggingStatus
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLoggingStatusResult"
      (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable DescribeLoggingStatus

instance Prelude.NFData DescribeLoggingStatus

instance Prelude.ToHeaders DescribeLoggingStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeLoggingStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeLoggingStatus where
  toQuery DescribeLoggingStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeLoggingStatus" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Prelude.=: clusterIdentifier
      ]
