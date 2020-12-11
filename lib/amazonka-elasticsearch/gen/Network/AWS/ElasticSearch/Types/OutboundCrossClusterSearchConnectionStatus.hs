-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
  ( OutboundCrossClusterSearchConnectionStatus (..),

    -- * Smart constructor
    mkOutboundCrossClusterSearchConnectionStatus,

    -- * Lenses
    occscsMessage,
    occscsStatusCode,
  )
where

import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the connection status of an outbound cross-cluster search connection.
--
-- /See:/ 'mkOutboundCrossClusterSearchConnectionStatus' smart constructor.
data OutboundCrossClusterSearchConnectionStatus = OutboundCrossClusterSearchConnectionStatus'
  { message ::
      Lude.Maybe
        Lude.Text,
    statusCode ::
      Lude.Maybe
        OutboundCrossClusterSearchConnectionStatusCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutboundCrossClusterSearchConnectionStatus' with the minimum fields required to make a request.
--
-- * 'message' - Specifies verbose information for the outbound connection status.
-- * 'statusCode' - The state code for outbound connection. This can be one of the following:
--
--
--     * VALIDATING: The outbound connection request is being validated.
--
--     * VALIDATION_FAILED: Validation failed for the connection request.
--
--     * PENDING_ACCEPTANCE: Outbound connection request is validated and is not yet accepted by destination domain owner.
--
--     * PROVISIONING: Outbound connection request is in process.
--
--     * ACTIVE: Outbound connection is active and ready to use.
--
--     * REJECTED: Outbound connection request is rejected by destination domain owner.
--
--     * DELETING: Outbound connection deletion is in progress.
--
--     * DELETED: Outbound connection is deleted and cannot be used further.
mkOutboundCrossClusterSearchConnectionStatus ::
  OutboundCrossClusterSearchConnectionStatus
mkOutboundCrossClusterSearchConnectionStatus =
  OutboundCrossClusterSearchConnectionStatus'
    { message =
        Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | Specifies verbose information for the outbound connection status.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscsMessage :: Lens.Lens' OutboundCrossClusterSearchConnectionStatus (Lude.Maybe Lude.Text)
occscsMessage = Lens.lens (message :: OutboundCrossClusterSearchConnectionStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: OutboundCrossClusterSearchConnectionStatus)
{-# DEPRECATED occscsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The state code for outbound connection. This can be one of the following:
--
--
--     * VALIDATING: The outbound connection request is being validated.
--
--     * VALIDATION_FAILED: Validation failed for the connection request.
--
--     * PENDING_ACCEPTANCE: Outbound connection request is validated and is not yet accepted by destination domain owner.
--
--     * PROVISIONING: Outbound connection request is in process.
--
--     * ACTIVE: Outbound connection is active and ready to use.
--
--     * REJECTED: Outbound connection request is rejected by destination domain owner.
--
--     * DELETING: Outbound connection deletion is in progress.
--
--     * DELETED: Outbound connection is deleted and cannot be used further.
--
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
occscsStatusCode :: Lens.Lens' OutboundCrossClusterSearchConnectionStatus (Lude.Maybe OutboundCrossClusterSearchConnectionStatusCode)
occscsStatusCode = Lens.lens (statusCode :: OutboundCrossClusterSearchConnectionStatus -> Lude.Maybe OutboundCrossClusterSearchConnectionStatusCode) (\s a -> s {statusCode = a} :: OutboundCrossClusterSearchConnectionStatus)
{-# DEPRECATED occscsStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON OutboundCrossClusterSearchConnectionStatus where
  parseJSON =
    Lude.withObject
      "OutboundCrossClusterSearchConnectionStatus"
      ( \x ->
          OutboundCrossClusterSearchConnectionStatus'
            Lude.<$> (x Lude..:? "Message") Lude.<*> (x Lude..:? "StatusCode")
      )
