{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeLoggingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes whether information, such as queries and connection attempts, is being logged for the specified Amazon Redshift cluster.
module Network.AWS.Redshift.DescribeLoggingStatus
  ( -- * Creating a request
    DescribeLoggingStatus (..),
    mkDescribeLoggingStatus,

    -- ** Request lenses
    dlsClusterIdentifier,

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
-- /See:/ 'mkDescribeLoggingStatus' smart constructor.
newtype DescribeLoggingStatus = DescribeLoggingStatus'
  { -- | The identifier of the cluster from which to get the logging status.
    --
    -- Example: @examplecluster@
    clusterIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeLoggingStatus' value with any optional fields omitted.
mkDescribeLoggingStatus ::
  -- | 'clusterIdentifier'
  Types.String ->
  DescribeLoggingStatus
mkDescribeLoggingStatus clusterIdentifier =
  DescribeLoggingStatus' {clusterIdentifier}

-- | The identifier of the cluster from which to get the logging status.
--
-- Example: @examplecluster@
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsClusterIdentifier :: Lens.Lens' DescribeLoggingStatus Types.String
dlsClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED dlsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Core.AWSRequest DescribeLoggingStatus where
  type Rs DescribeLoggingStatus = Types.LoggingStatus
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
            ( Core.pure ("Action", "DescribeLoggingStatus")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeLoggingStatusResult"
      (\s h x -> Core.parseXML x)
