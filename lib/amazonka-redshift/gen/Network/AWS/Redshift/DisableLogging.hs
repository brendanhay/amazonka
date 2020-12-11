{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DisableLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops logging information, such as queries and connection attempts, for the specified Amazon Redshift cluster.
module Network.AWS.Redshift.DisableLogging
  ( -- * Creating a request
    DisableLogging (..),
    mkDisableLogging,

    -- ** Request lenses
    dlClusterIdentifier,

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
-- /See:/ 'mkDisableLogging' smart constructor.
newtype DisableLogging = DisableLogging'
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

-- | Creates a value of 'DisableLogging' with the minimum fields required to make a request.
--
-- * 'clusterIdentifier' - The identifier of the cluster on which logging is to be stopped.
--
-- Example: @examplecluster@
mkDisableLogging ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  DisableLogging
mkDisableLogging pClusterIdentifier_ =
  DisableLogging' {clusterIdentifier = pClusterIdentifier_}

-- | The identifier of the cluster on which logging is to be stopped.
--
-- Example: @examplecluster@
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlClusterIdentifier :: Lens.Lens' DisableLogging Lude.Text
dlClusterIdentifier = Lens.lens (clusterIdentifier :: DisableLogging -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: DisableLogging)
{-# DEPRECATED dlClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.AWSRequest DisableLogging where
  type Rs DisableLogging = LoggingStatus
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DisableLoggingResult"
      (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders DisableLogging where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableLogging where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableLogging where
  toQuery DisableLogging' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DisableLogging" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]
