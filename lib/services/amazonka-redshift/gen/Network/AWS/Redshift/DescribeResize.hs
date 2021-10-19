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
-- Module      : Network.AWS.Redshift.DescribeResize
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the last resize operation for the specified
-- cluster. If no resize operation has ever been initiated for the
-- specified cluster, a @HTTP 404@ error is returned. If a resize operation
-- was initiated and completed, the status of the resize remains as
-- @SUCCEEDED@ until the next resize.
--
-- A resize operation can be requested using ModifyCluster and specifying a
-- different number or type of nodes for the cluster.
module Network.AWS.Redshift.DescribeResize
  ( -- * Creating a Request
    DescribeResize (..),
    newDescribeResize,

    -- * Request Lenses
    describeResize_clusterIdentifier,

    -- * Destructuring the Response
    ResizeProgressMessage (..),
    newResizeProgressMessage,

    -- * Response Lenses
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_status,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_resizeType,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_totalResizeDataInMegaBytes,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_message,
    resizeProgressMessage_elapsedTimeInSeconds,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeResize' smart constructor.
data DescribeResize = DescribeResize'
  { -- | The unique identifier of a cluster whose resize progress you are
    -- requesting. This parameter is case-sensitive.
    --
    -- By default, resize operations for all clusters defined for an Amazon Web
    -- Services account are returned.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResize' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'describeResize_clusterIdentifier' - The unique identifier of a cluster whose resize progress you are
-- requesting. This parameter is case-sensitive.
--
-- By default, resize operations for all clusters defined for an Amazon Web
-- Services account are returned.
newDescribeResize ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  DescribeResize
newDescribeResize pClusterIdentifier_ =
  DescribeResize'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The unique identifier of a cluster whose resize progress you are
-- requesting. This parameter is case-sensitive.
--
-- By default, resize operations for all clusters defined for an Amazon Web
-- Services account are returned.
describeResize_clusterIdentifier :: Lens.Lens' DescribeResize Prelude.Text
describeResize_clusterIdentifier = Lens.lens (\DescribeResize' {clusterIdentifier} -> clusterIdentifier) (\s@DescribeResize' {} a -> s {clusterIdentifier = a} :: DescribeResize)

instance Core.AWSRequest DescribeResize where
  type
    AWSResponse DescribeResize =
      ResizeProgressMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeResizeResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable DescribeResize

instance Prelude.NFData DescribeResize

instance Core.ToHeaders DescribeResize where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeResize where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeResize where
  toQuery DescribeResize' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeResize" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]
