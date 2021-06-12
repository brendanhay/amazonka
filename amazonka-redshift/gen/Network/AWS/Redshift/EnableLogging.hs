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
-- Module      : Network.AWS.Redshift.EnableLogging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts logging information, such as queries and connection attempts, for
-- the specified Amazon Redshift cluster.
module Network.AWS.Redshift.EnableLogging
  ( -- * Creating a Request
    EnableLogging (..),
    newEnableLogging,

    -- * Request Lenses
    enableLogging_s3KeyPrefix,
    enableLogging_clusterIdentifier,
    enableLogging_bucketName,

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
-- /See:/ 'newEnableLogging' smart constructor.
data EnableLogging = EnableLogging'
  { -- | The prefix applied to the log file names.
    --
    -- Constraints:
    --
    -- -   Cannot exceed 512 characters
    --
    -- -   Cannot contain spaces( ), double quotes (\"), single quotes (\'), a
    --     backslash (\\), or control characters. The hexadecimal codes for
    --     invalid characters are:
    --
    --     -   x00 to x20
    --
    --     -   x22
    --
    --     -   x27
    --
    --     -   x5c
    --
    --     -   x7f or larger
    s3KeyPrefix :: Core.Maybe Core.Text,
    -- | The identifier of the cluster on which logging is to be started.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Core.Text,
    -- | The name of an existing S3 bucket where the log files are to be stored.
    --
    -- Constraints:
    --
    -- -   Must be in the same region as the cluster
    --
    -- -   The cluster must have read bucket and put object permissions
    bucketName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableLogging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KeyPrefix', 'enableLogging_s3KeyPrefix' - The prefix applied to the log file names.
--
-- Constraints:
--
-- -   Cannot exceed 512 characters
--
-- -   Cannot contain spaces( ), double quotes (\"), single quotes (\'), a
--     backslash (\\), or control characters. The hexadecimal codes for
--     invalid characters are:
--
--     -   x00 to x20
--
--     -   x22
--
--     -   x27
--
--     -   x5c
--
--     -   x7f or larger
--
-- 'clusterIdentifier', 'enableLogging_clusterIdentifier' - The identifier of the cluster on which logging is to be started.
--
-- Example: @examplecluster@
--
-- 'bucketName', 'enableLogging_bucketName' - The name of an existing S3 bucket where the log files are to be stored.
--
-- Constraints:
--
-- -   Must be in the same region as the cluster
--
-- -   The cluster must have read bucket and put object permissions
newEnableLogging ::
  -- | 'clusterIdentifier'
  Core.Text ->
  -- | 'bucketName'
  Core.Text ->
  EnableLogging
newEnableLogging pClusterIdentifier_ pBucketName_ =
  EnableLogging'
    { s3KeyPrefix = Core.Nothing,
      clusterIdentifier = pClusterIdentifier_,
      bucketName = pBucketName_
    }

-- | The prefix applied to the log file names.
--
-- Constraints:
--
-- -   Cannot exceed 512 characters
--
-- -   Cannot contain spaces( ), double quotes (\"), single quotes (\'), a
--     backslash (\\), or control characters. The hexadecimal codes for
--     invalid characters are:
--
--     -   x00 to x20
--
--     -   x22
--
--     -   x27
--
--     -   x5c
--
--     -   x7f or larger
enableLogging_s3KeyPrefix :: Lens.Lens' EnableLogging (Core.Maybe Core.Text)
enableLogging_s3KeyPrefix = Lens.lens (\EnableLogging' {s3KeyPrefix} -> s3KeyPrefix) (\s@EnableLogging' {} a -> s {s3KeyPrefix = a} :: EnableLogging)

-- | The identifier of the cluster on which logging is to be started.
--
-- Example: @examplecluster@
enableLogging_clusterIdentifier :: Lens.Lens' EnableLogging Core.Text
enableLogging_clusterIdentifier = Lens.lens (\EnableLogging' {clusterIdentifier} -> clusterIdentifier) (\s@EnableLogging' {} a -> s {clusterIdentifier = a} :: EnableLogging)

-- | The name of an existing S3 bucket where the log files are to be stored.
--
-- Constraints:
--
-- -   Must be in the same region as the cluster
--
-- -   The cluster must have read bucket and put object permissions
enableLogging_bucketName :: Lens.Lens' EnableLogging Core.Text
enableLogging_bucketName = Lens.lens (\EnableLogging' {bucketName} -> bucketName) (\s@EnableLogging' {} a -> s {bucketName = a} :: EnableLogging)

instance Core.AWSRequest EnableLogging where
  type AWSResponse EnableLogging = LoggingStatus
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "EnableLoggingResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable EnableLogging

instance Core.NFData EnableLogging

instance Core.ToHeaders EnableLogging where
  toHeaders = Core.const Core.mempty

instance Core.ToPath EnableLogging where
  toPath = Core.const "/"

instance Core.ToQuery EnableLogging where
  toQuery EnableLogging' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("EnableLogging" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "S3KeyPrefix" Core.=: s3KeyPrefix,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "BucketName" Core.=: bucketName
      ]
