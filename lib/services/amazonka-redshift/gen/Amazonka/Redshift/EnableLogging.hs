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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    enableLogging_clusterIdentifier,
    enableLogging_bucketName,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
    -- | The identifier of the cluster on which logging is to be started.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Prelude.Text,
    -- | The name of an existing S3 bucket where the log files are to be stored.
    --
    -- Constraints:
    --
    -- -   Must be in the same region as the cluster
    --
    -- -   The cluster must have read bucket and put object permissions
    bucketName :: Prelude.Text
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
  Prelude.Text ->
  -- | 'bucketName'
  Prelude.Text ->
  EnableLogging
newEnableLogging pClusterIdentifier_ pBucketName_ =
  EnableLogging'
    { s3KeyPrefix = Prelude.Nothing,
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
enableLogging_s3KeyPrefix :: Lens.Lens' EnableLogging (Prelude.Maybe Prelude.Text)
enableLogging_s3KeyPrefix = Lens.lens (\EnableLogging' {s3KeyPrefix} -> s3KeyPrefix) (\s@EnableLogging' {} a -> s {s3KeyPrefix = a} :: EnableLogging)

-- | The identifier of the cluster on which logging is to be started.
--
-- Example: @examplecluster@
enableLogging_clusterIdentifier :: Lens.Lens' EnableLogging Prelude.Text
enableLogging_clusterIdentifier = Lens.lens (\EnableLogging' {clusterIdentifier} -> clusterIdentifier) (\s@EnableLogging' {} a -> s {clusterIdentifier = a} :: EnableLogging)

-- | The name of an existing S3 bucket where the log files are to be stored.
--
-- Constraints:
--
-- -   Must be in the same region as the cluster
--
-- -   The cluster must have read bucket and put object permissions
enableLogging_bucketName :: Lens.Lens' EnableLogging Prelude.Text
enableLogging_bucketName = Lens.lens (\EnableLogging' {bucketName} -> bucketName) (\s@EnableLogging' {} a -> s {bucketName = a} :: EnableLogging)

instance Core.AWSRequest EnableLogging where
  type AWSResponse EnableLogging = LoggingStatus
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "EnableLoggingResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable EnableLogging where
  hashWithSalt _salt EnableLogging' {..} =
    _salt `Prelude.hashWithSalt` s3KeyPrefix
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData EnableLogging where
  rnf EnableLogging' {..} =
    Prelude.rnf s3KeyPrefix
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf bucketName

instance Core.ToHeaders EnableLogging where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath EnableLogging where
  toPath = Prelude.const "/"

instance Core.ToQuery EnableLogging where
  toQuery EnableLogging' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("EnableLogging" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "S3KeyPrefix" Core.=: s3KeyPrefix,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "BucketName" Core.=: bucketName
      ]
