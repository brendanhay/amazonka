{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.DeleteEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the endpoint for a device and mobile app from Amazon SNS. This action is idempotent. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
-- When you delete an endpoint that is also subscribed to a topic, then you must also unsubscribe the endpoint from the topic.
module Network.AWS.SNS.DeleteEndpoint
  ( -- * Creating a request
    DeleteEndpoint (..),
    mkDeleteEndpoint,

    -- ** Request lenses
    deEndpointARN,

    -- * Destructuring the response
    DeleteEndpointResponse (..),
    mkDeleteEndpointResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for DeleteEndpoint action.
--
-- /See:/ 'mkDeleteEndpoint' smart constructor.
newtype DeleteEndpoint = DeleteEndpoint' {endpointARN :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEndpoint' with the minimum fields required to make a request.
--
-- * 'endpointARN' - EndpointArn of endpoint to delete.
mkDeleteEndpoint ::
  -- | 'endpointARN'
  Lude.Text ->
  DeleteEndpoint
mkDeleteEndpoint pEndpointARN_ =
  DeleteEndpoint' {endpointARN = pEndpointARN_}

-- | EndpointArn of endpoint to delete.
--
-- /Note:/ Consider using 'endpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deEndpointARN :: Lens.Lens' DeleteEndpoint Lude.Text
deEndpointARN = Lens.lens (endpointARN :: DeleteEndpoint -> Lude.Text) (\s a -> s {endpointARN = a} :: DeleteEndpoint)
{-# DEPRECATED deEndpointARN "Use generic-lens or generic-optics with 'endpointARN' instead." #-}

instance Lude.AWSRequest DeleteEndpoint where
  type Rs DeleteEndpoint = DeleteEndpointResponse
  request = Req.postQuery snsService
  response = Res.receiveNull DeleteEndpointResponse'

instance Lude.ToHeaders DeleteEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEndpoint where
  toQuery DeleteEndpoint' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteEndpoint" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "EndpointArn" Lude.=: endpointARN
      ]

-- | /See:/ 'mkDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEndpointResponse' with the minimum fields required to make a request.
mkDeleteEndpointResponse ::
  DeleteEndpointResponse
mkDeleteEndpointResponse = DeleteEndpointResponse'
