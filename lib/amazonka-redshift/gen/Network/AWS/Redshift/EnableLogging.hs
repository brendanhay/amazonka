{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.EnableLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts logging information, such as queries and connection attempts, for the specified Amazon Redshift cluster.
module Network.AWS.Redshift.EnableLogging
  ( -- * Creating a request
    EnableLogging (..),
    mkEnableLogging,

    -- ** Request lenses
    elS3KeyPrefix,
    elClusterIdentifier,
    elBucketName,

    -- * Destructuring the response
    LoggingStatus (..),
    mkLoggingStatus,

    -- ** Response lenses
    lsLastFailureTime,
    lsLastSuccessfulDeliveryTime,
    lsS3KeyPrefix,
    lsBucketName,
    lsLoggingEnabled,
    lsLastFailureMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkEnableLogging' smart constructor.
data EnableLogging = EnableLogging'
  { -- | The prefix applied to the log file names.
    --
    -- Constraints:
    --
    --     * Cannot exceed 512 characters
    --
    --
    --     * Cannot contain spaces( ), double quotes ("), single quotes ('), a backslash (\), or control characters. The hexadecimal codes for invalid characters are:
    --
    --     * x00 to x20
    --
    --
    --     * x22
    --
    --
    --     * x27
    --
    --
    --     * x5c
    --
    --
    --     * x7f or larger
    s3KeyPrefix :: Lude.Maybe Lude.Text,
    -- | The identifier of the cluster on which logging is to be started.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Lude.Text,
    -- | The name of an existing S3 bucket where the log files are to be stored.
    --
    -- Constraints:
    --
    --     * Must be in the same region as the cluster
    --
    --
    --     * The cluster must have read bucket and put object permissions
    bucketName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableLogging' with the minimum fields required to make a request.
--
-- * 's3KeyPrefix' - The prefix applied to the log file names.
--
-- Constraints:
--
--     * Cannot exceed 512 characters
--
--
--     * Cannot contain spaces( ), double quotes ("), single quotes ('), a backslash (\), or control characters. The hexadecimal codes for invalid characters are:
--
--     * x00 to x20
--
--
--     * x22
--
--
--     * x27
--
--
--     * x5c
--
--
--     * x7f or larger
--
--
--
--
-- * 'clusterIdentifier' - The identifier of the cluster on which logging is to be started.
--
-- Example: @examplecluster@
-- * 'bucketName' - The name of an existing S3 bucket where the log files are to be stored.
--
-- Constraints:
--
--     * Must be in the same region as the cluster
--
--
--     * The cluster must have read bucket and put object permissions
mkEnableLogging ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  -- | 'bucketName'
  Lude.Text ->
  EnableLogging
mkEnableLogging pClusterIdentifier_ pBucketName_ =
  EnableLogging'
    { s3KeyPrefix = Lude.Nothing,
      clusterIdentifier = pClusterIdentifier_,
      bucketName = pBucketName_
    }

-- | The prefix applied to the log file names.
--
-- Constraints:
--
--     * Cannot exceed 512 characters
--
--
--     * Cannot contain spaces( ), double quotes ("), single quotes ('), a backslash (\), or control characters. The hexadecimal codes for invalid characters are:
--
--     * x00 to x20
--
--
--     * x22
--
--
--     * x27
--
--
--     * x5c
--
--
--     * x7f or larger
--
--
--
--
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elS3KeyPrefix :: Lens.Lens' EnableLogging (Lude.Maybe Lude.Text)
elS3KeyPrefix = Lens.lens (s3KeyPrefix :: EnableLogging -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: EnableLogging)
{-# DEPRECATED elS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | The identifier of the cluster on which logging is to be started.
--
-- Example: @examplecluster@
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elClusterIdentifier :: Lens.Lens' EnableLogging Lude.Text
elClusterIdentifier = Lens.lens (clusterIdentifier :: EnableLogging -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: EnableLogging)
{-# DEPRECATED elClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The name of an existing S3 bucket where the log files are to be stored.
--
-- Constraints:
--
--     * Must be in the same region as the cluster
--
--
--     * The cluster must have read bucket and put object permissions
--
--
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elBucketName :: Lens.Lens' EnableLogging Lude.Text
elBucketName = Lens.lens (bucketName :: EnableLogging -> Lude.Text) (\s a -> s {bucketName = a} :: EnableLogging)
{-# DEPRECATED elBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

instance Lude.AWSRequest EnableLogging where
  type Rs EnableLogging = LoggingStatus
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "EnableLoggingResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders EnableLogging where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableLogging where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableLogging where
  toQuery EnableLogging' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EnableLogging" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "S3KeyPrefix" Lude.=: s3KeyPrefix,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "BucketName" Lude.=: bucketName
      ]
