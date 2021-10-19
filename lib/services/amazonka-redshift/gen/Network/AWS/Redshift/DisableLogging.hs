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
-- Module      : Network.AWS.Redshift.DisableLogging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops logging information, such as queries and connection attempts, for
-- the specified Amazon Redshift cluster.
module Network.AWS.Redshift.DisableLogging
  ( -- * Creating a Request
    DisableLogging (..),
    newDisableLogging,

    -- * Request Lenses
    disableLogging_clusterIdentifier,

    -- * Destructuring the Response
    LoggingStatus (..),
    newLoggingStatus,

    -- * Response Lenses
    loggingStatus_lastFailureTime,
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_bucketName,
    loggingStatus_loggingEnabled,
    loggingStatus_lastFailureMessage,
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
-- /See:/ 'newDisableLogging' smart constructor.
data DisableLogging = DisableLogging'
  { -- | The identifier of the cluster on which logging is to be stopped.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableLogging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'disableLogging_clusterIdentifier' - The identifier of the cluster on which logging is to be stopped.
--
-- Example: @examplecluster@
newDisableLogging ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  DisableLogging
newDisableLogging pClusterIdentifier_ =
  DisableLogging'
    { clusterIdentifier =
        pClusterIdentifier_
    }

-- | The identifier of the cluster on which logging is to be stopped.
--
-- Example: @examplecluster@
disableLogging_clusterIdentifier :: Lens.Lens' DisableLogging Prelude.Text
disableLogging_clusterIdentifier = Lens.lens (\DisableLogging' {clusterIdentifier} -> clusterIdentifier) (\s@DisableLogging' {} a -> s {clusterIdentifier = a} :: DisableLogging)

instance Core.AWSRequest DisableLogging where
  type AWSResponse DisableLogging = LoggingStatus
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DisableLoggingResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable DisableLogging

instance Prelude.NFData DisableLogging

instance Core.ToHeaders DisableLogging where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DisableLogging where
  toPath = Prelude.const "/"

instance Core.ToQuery DisableLogging where
  toQuery DisableLogging' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DisableLogging" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "ClusterIdentifier" Core.=: clusterIdentifier
      ]
