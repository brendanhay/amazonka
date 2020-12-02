{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeResize
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the last resize operation for the specified cluster. If no resize operation has ever been initiated for the specified cluster, a @HTTP 404@ error is returned. If a resize operation was initiated and completed, the status of the resize remains as @SUCCEEDED@ until the next resize.
--
--
-- A resize operation can be requested using 'ModifyCluster' and specifying a different number or type of nodes for the cluster.
module Network.AWS.Redshift.DescribeResize
  ( -- * Creating a Request
    describeResize,
    DescribeResize,

    -- * Request Lenses
    drClusterIdentifier,

    -- * Destructuring the Response
    resizeProgressMessage,
    ResizeProgressMessage,

    -- * Response Lenses
    rpmImportTablesNotStarted,
    rpmStatus,
    rpmEstimatedTimeToCompletionInSeconds,
    rpmAvgResizeRateInMegaBytesPerSecond,
    rpmTargetNumberOfNodes,
    rpmTargetEncryptionType,
    rpmTargetNodeType,
    rpmImportTablesInProgress,
    rpmResizeType,
    rpmImportTablesCompleted,
    rpmProgressInMegaBytes,
    rpmDataTransferProgressPercent,
    rpmTotalResizeDataInMegaBytes,
    rpmTargetClusterType,
    rpmMessage,
    rpmElapsedTimeInSeconds,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeResize' smart constructor.
newtype DescribeResize = DescribeResize'
  { _drClusterIdentifier ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeResize' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drClusterIdentifier' - The unique identifier of a cluster whose resize progress you are requesting. This parameter is case-sensitive. By default, resize operations for all clusters defined for an AWS account are returned.
describeResize ::
  -- | 'drClusterIdentifier'
  Text ->
  DescribeResize
describeResize pClusterIdentifier_ =
  DescribeResize' {_drClusterIdentifier = pClusterIdentifier_}

-- | The unique identifier of a cluster whose resize progress you are requesting. This parameter is case-sensitive. By default, resize operations for all clusters defined for an AWS account are returned.
drClusterIdentifier :: Lens' DescribeResize Text
drClusterIdentifier = lens _drClusterIdentifier (\s a -> s {_drClusterIdentifier = a})

instance AWSRequest DescribeResize where
  type Rs DescribeResize = ResizeProgressMessage
  request = postQuery redshift
  response =
    receiveXMLWrapper "DescribeResizeResult" (\s h x -> parseXML x)

instance Hashable DescribeResize

instance NFData DescribeResize

instance ToHeaders DescribeResize where
  toHeaders = const mempty

instance ToPath DescribeResize where
  toPath = const "/"

instance ToQuery DescribeResize where
  toQuery DescribeResize' {..} =
    mconcat
      [ "Action" =: ("DescribeResize" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "ClusterIdentifier" =: _drClusterIdentifier
      ]
