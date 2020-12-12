{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
  ( InboundCrossClusterSearchConnectionStatus (..),

    -- * Smart constructor
    mkInboundCrossClusterSearchConnectionStatus,

    -- * Lenses
    iccscsMessage,
    iccscsStatusCode,
  )
where

import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the coonection status of an inbound cross-cluster search connection.
--
-- /See:/ 'mkInboundCrossClusterSearchConnectionStatus' smart constructor.
data InboundCrossClusterSearchConnectionStatus = InboundCrossClusterSearchConnectionStatus'
  { message ::
      Lude.Maybe
        Lude.Text,
    statusCode ::
      Lude.Maybe
        InboundCrossClusterSearchConnectionStatusCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InboundCrossClusterSearchConnectionStatus' with the minimum fields required to make a request.
--
-- * 'message' - Specifies verbose information for the inbound connection status.
-- * 'statusCode' - The state code for inbound connection. This can be one of the following:
--
--
--     * PENDING_ACCEPTANCE: Inbound connection is not yet accepted by destination domain owner.
--
--     * APPROVED: Inbound connection is pending acceptance by destination domain owner.
--
--     * REJECTING: Inbound connection rejection is in process.
--
--     * REJECTED: Inbound connection is rejected.
--
--     * DELETING: Inbound connection deletion is in progress.
--
--     * DELETED: Inbound connection is deleted and cannot be used further.
mkInboundCrossClusterSearchConnectionStatus ::
  InboundCrossClusterSearchConnectionStatus
mkInboundCrossClusterSearchConnectionStatus =
  InboundCrossClusterSearchConnectionStatus'
    { message =
        Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | Specifies verbose information for the inbound connection status.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscsMessage :: Lens.Lens' InboundCrossClusterSearchConnectionStatus (Lude.Maybe Lude.Text)
iccscsMessage = Lens.lens (message :: InboundCrossClusterSearchConnectionStatus -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: InboundCrossClusterSearchConnectionStatus)
{-# DEPRECATED iccscsMessage "Use generic-lens or generic-optics with 'message' instead." #-}

-- | The state code for inbound connection. This can be one of the following:
--
--
--     * PENDING_ACCEPTANCE: Inbound connection is not yet accepted by destination domain owner.
--
--     * APPROVED: Inbound connection is pending acceptance by destination domain owner.
--
--     * REJECTING: Inbound connection rejection is in process.
--
--     * REJECTED: Inbound connection is rejected.
--
--     * DELETING: Inbound connection deletion is in progress.
--
--     * DELETED: Inbound connection is deleted and cannot be used further.
--
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iccscsStatusCode :: Lens.Lens' InboundCrossClusterSearchConnectionStatus (Lude.Maybe InboundCrossClusterSearchConnectionStatusCode)
iccscsStatusCode = Lens.lens (statusCode :: InboundCrossClusterSearchConnectionStatus -> Lude.Maybe InboundCrossClusterSearchConnectionStatusCode) (\s a -> s {statusCode = a} :: InboundCrossClusterSearchConnectionStatus)
{-# DEPRECATED iccscsStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON InboundCrossClusterSearchConnectionStatus where
  parseJSON =
    Lude.withObject
      "InboundCrossClusterSearchConnectionStatus"
      ( \x ->
          InboundCrossClusterSearchConnectionStatus'
            Lude.<$> (x Lude..:? "Message") Lude.<*> (x Lude..:? "StatusCode")
      )
