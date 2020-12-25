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
    elClusterIdentifier,
    elBucketName,
    elS3KeyPrefix,

    -- * Destructuring the response
    Types.LoggingStatus (..),
    Types.mkLoggingStatus,

    -- ** Response lenses
    Types.lsBucketName,
    Types.lsLastFailureMessage,
    Types.lsLastFailureTime,
    Types.lsLastSuccessfulDeliveryTime,
    Types.lsLoggingEnabled,
    Types.lsS3KeyPrefix,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkEnableLogging' smart constructor.
data EnableLogging = EnableLogging'
  { -- | The identifier of the cluster on which logging is to be started.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Types.String,
    -- | The name of an existing S3 bucket where the log files are to be stored.
    --
    -- Constraints:
    --
    --     * Must be in the same region as the cluster
    --
    --
    --     * The cluster must have read bucket and put object permissions
    bucketName :: Types.String,
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
    s3KeyPrefix :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableLogging' value with any optional fields omitted.
mkEnableLogging ::
  -- | 'clusterIdentifier'
  Types.String ->
  -- | 'bucketName'
  Types.String ->
  EnableLogging
mkEnableLogging clusterIdentifier bucketName =
  EnableLogging'
    { clusterIdentifier,
      bucketName,
      s3KeyPrefix = Core.Nothing
    }

-- | The identifier of the cluster on which logging is to be started.
--
-- Example: @examplecluster@
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elClusterIdentifier :: Lens.Lens' EnableLogging Types.String
elClusterIdentifier = Lens.field @"clusterIdentifier"
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
elBucketName :: Lens.Lens' EnableLogging Types.String
elBucketName = Lens.field @"bucketName"
{-# DEPRECATED elBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

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
elS3KeyPrefix :: Lens.Lens' EnableLogging (Core.Maybe Types.String)
elS3KeyPrefix = Lens.field @"s3KeyPrefix"
{-# DEPRECATED elS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

instance Core.AWSRequest EnableLogging where
  type Rs EnableLogging = Types.LoggingStatus
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "EnableLogging")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
                Core.<> (Core.toQueryValue "BucketName" bucketName)
                Core.<> (Core.toQueryValue "S3KeyPrefix" Core.<$> s3KeyPrefix)
            )
      }
  response =
    Response.receiveXMLWrapper
      "EnableLoggingResult"
      (\s h x -> Core.parseXML x)
