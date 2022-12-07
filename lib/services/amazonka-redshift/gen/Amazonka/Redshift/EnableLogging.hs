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
-- Module      : Amazonka.Redshift.EnableLogging
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts logging information, such as queries and connection attempts, for
-- the specified Amazon Redshift cluster.
module Amazonka.Redshift.EnableLogging
  ( -- * Creating a Request
    EnableLogging (..),
    newEnableLogging,

    -- * Request Lenses
    enableLogging_s3KeyPrefix,
    enableLogging_logExports,
    enableLogging_bucketName,
    enableLogging_logDestinationType,
    enableLogging_clusterIdentifier,

    -- * Destructuring the Response
    LoggingStatus (..),
    newLoggingStatus,

    -- * Response Lenses
    loggingStatus_lastSuccessfulDeliveryTime,
    loggingStatus_s3KeyPrefix,
    loggingStatus_lastFailureMessage,
    loggingStatus_loggingEnabled,
    loggingStatus_logExports,
    loggingStatus_bucketName,
    loggingStatus_lastFailureTime,
    loggingStatus_logDestinationType,
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
    s3KeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The collection of exported log types. Log types include the connection
    -- log, user log and user activity log.
    logExports :: Prelude.Maybe [Prelude.Text],
    -- | The name of an existing S3 bucket where the log files are to be stored.
    --
    -- Constraints:
    --
    -- -   Must be in the same region as the cluster
    --
    -- -   The cluster must have read bucket and put object permissions
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The log destination type. An enum with possible values of @s3@ and
    -- @cloudwatch@.
    logDestinationType :: Prelude.Maybe LogDestinationType,
    -- | The identifier of the cluster on which logging is to be started.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'logExports', 'enableLogging_logExports' - The collection of exported log types. Log types include the connection
-- log, user log and user activity log.
--
-- 'bucketName', 'enableLogging_bucketName' - The name of an existing S3 bucket where the log files are to be stored.
--
-- Constraints:
--
-- -   Must be in the same region as the cluster
--
-- -   The cluster must have read bucket and put object permissions
--
-- 'logDestinationType', 'enableLogging_logDestinationType' - The log destination type. An enum with possible values of @s3@ and
-- @cloudwatch@.
--
-- 'clusterIdentifier', 'enableLogging_clusterIdentifier' - The identifier of the cluster on which logging is to be started.
--
-- Example: @examplecluster@
newEnableLogging ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  EnableLogging
newEnableLogging pClusterIdentifier_ =
  EnableLogging'
    { s3KeyPrefix = Prelude.Nothing,
      logExports = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      logDestinationType = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
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
enableLogging_s3KeyPrefix :: Lens.Lens' EnableLogging (Prelude.Maybe Prelude.Text)
enableLogging_s3KeyPrefix = Lens.lens (\EnableLogging' {s3KeyPrefix} -> s3KeyPrefix) (\s@EnableLogging' {} a -> s {s3KeyPrefix = a} :: EnableLogging)

-- | The collection of exported log types. Log types include the connection
-- log, user log and user activity log.
enableLogging_logExports :: Lens.Lens' EnableLogging (Prelude.Maybe [Prelude.Text])
enableLogging_logExports = Lens.lens (\EnableLogging' {logExports} -> logExports) (\s@EnableLogging' {} a -> s {logExports = a} :: EnableLogging) Prelude.. Lens.mapping Lens.coerced

-- | The name of an existing S3 bucket where the log files are to be stored.
--
-- Constraints:
--
-- -   Must be in the same region as the cluster
--
-- -   The cluster must have read bucket and put object permissions
enableLogging_bucketName :: Lens.Lens' EnableLogging (Prelude.Maybe Prelude.Text)
enableLogging_bucketName = Lens.lens (\EnableLogging' {bucketName} -> bucketName) (\s@EnableLogging' {} a -> s {bucketName = a} :: EnableLogging)

-- | The log destination type. An enum with possible values of @s3@ and
-- @cloudwatch@.
enableLogging_logDestinationType :: Lens.Lens' EnableLogging (Prelude.Maybe LogDestinationType)
enableLogging_logDestinationType = Lens.lens (\EnableLogging' {logDestinationType} -> logDestinationType) (\s@EnableLogging' {} a -> s {logDestinationType = a} :: EnableLogging)

-- | The identifier of the cluster on which logging is to be started.
--
-- Example: @examplecluster@
enableLogging_clusterIdentifier :: Lens.Lens' EnableLogging Prelude.Text
enableLogging_clusterIdentifier = Lens.lens (\EnableLogging' {clusterIdentifier} -> clusterIdentifier) (\s@EnableLogging' {} a -> s {clusterIdentifier = a} :: EnableLogging)

instance Core.AWSRequest EnableLogging where
  type AWSResponse EnableLogging = LoggingStatus
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "EnableLoggingResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable EnableLogging where
  hashWithSalt _salt EnableLogging' {..} =
    _salt `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` logExports
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` logDestinationType
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData EnableLogging where
  rnf EnableLogging' {..} =
    Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf logExports
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf logDestinationType
      `Prelude.seq` Prelude.rnf clusterIdentifier

instance Data.ToHeaders EnableLogging where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath EnableLogging where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableLogging where
  toQuery EnableLogging' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("EnableLogging" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "S3KeyPrefix" Data.=: s3KeyPrefix,
        "LogExports"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> logExports),
        "BucketName" Data.=: bucketName,
        "LogDestinationType" Data.=: logDestinationType,
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]
