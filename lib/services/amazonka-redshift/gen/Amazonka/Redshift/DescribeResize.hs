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
-- Module      : Amazonka.Redshift.DescribeResize
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Redshift.DescribeResize
  ( -- * Creating a Request
    DescribeResize (..),
    newDescribeResize,

    -- * Request Lenses
    describeResize_clusterIdentifier,

    -- * Destructuring the Response
    ResizeProgressMessage (..),
    newResizeProgressMessage,

    -- * Response Lenses
    resizeProgressMessage_avgResizeRateInMegaBytesPerSecond,
    resizeProgressMessage_dataTransferProgressPercent,
    resizeProgressMessage_elapsedTimeInSeconds,
    resizeProgressMessage_estimatedTimeToCompletionInSeconds,
    resizeProgressMessage_importTablesCompleted,
    resizeProgressMessage_importTablesInProgress,
    resizeProgressMessage_importTablesNotStarted,
    resizeProgressMessage_message,
    resizeProgressMessage_progressInMegaBytes,
    resizeProgressMessage_resizeType,
    resizeProgressMessage_status,
    resizeProgressMessage_targetClusterType,
    resizeProgressMessage_targetEncryptionType,
    resizeProgressMessage_targetNodeType,
    resizeProgressMessage_targetNumberOfNodes,
    resizeProgressMessage_totalResizeDataInMegaBytes,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeResizeResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable DescribeResize where
  hashWithSalt _salt DescribeResize' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData DescribeResize where
  rnf DescribeResize' {..} =
    Prelude.rnf clusterIdentifier

instance Data.ToHeaders DescribeResize where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeResize where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeResize where
  toQuery DescribeResize' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeResize" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]
