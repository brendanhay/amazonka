{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- /See:/ 'mkDescribeLoggingStatus' smart constructor.
newtype DescribeLoggingStatus = DescribeLoggingStatus'
  { clusterIdentifier ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLoggingStatus' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The identifier of the cluster from which to get the logging status.
--
-- Example: @examplecluster@
mkDescribeLoggingStatus ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  DescribeLoggingStatus
mkDescribeLoggingStatus pClusterIdentifier_ =
  DescribeLoggingStatus' {clusterIdentifier = pClusterIdentifier_}

-- | The identifier of the cluster from which to get the logging status.
--
-- Example: @examplecluster@
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlsClusterIdentifier :: Lens.Lens' DescribeLoggingStatus Lude.Text
dlsClusterIdentifier = Lens.lens (clusterIdentifier :: DescribeLoggingStatus -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: DescribeLoggingStatus)
{-# DEPRECATED dlsClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest DescribeLoggingStatus where
  type Rs DescribeLoggingStatus = LoggingStatus
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeLoggingStatusResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders DescribeLoggingStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLoggingStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLoggingStatus where
  toQuery DescribeLoggingStatus' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeLoggingStatus" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]
