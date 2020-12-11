{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.DescribeContactFlow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified contact flow.
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
module Network.AWS.Connect.DescribeContactFlow
  ( -- * Creating a request
    DescribeContactFlow (..),
    mkDescribeContactFlow,

    -- ** Request lenses
    dcfInstanceId,
    dcfContactFlowId,

    -- * Destructuring the response
    DescribeContactFlowResponse (..),
    mkDescribeContactFlowResponse,

    -- ** Response lenses
    dcfrsContactFlow,
    dcfrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeContactFlow' smart constructor.
data DescribeContactFlow = DescribeContactFlow'
  { instanceId ::
      Lude.Text,
    contactFlowId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeContactFlow' with the minimum fields required to make a request.
--
-- * 'contactFlowId' - The identifier of the contact flow.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkDescribeContactFlow ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'contactFlowId'
  Lude.Text ->
  DescribeContactFlow
mkDescribeContactFlow pInstanceId_ pContactFlowId_ =
  DescribeContactFlow'
    { instanceId = pInstanceId_,
      contactFlowId = pContactFlowId_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfInstanceId :: Lens.Lens' DescribeContactFlow Lude.Text
dcfInstanceId = Lens.lens (instanceId :: DescribeContactFlow -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeContactFlow)
{-# DEPRECATED dcfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfContactFlowId :: Lens.Lens' DescribeContactFlow Lude.Text
dcfContactFlowId = Lens.lens (contactFlowId :: DescribeContactFlow -> Lude.Text) (\s a -> s {contactFlowId = a} :: DescribeContactFlow)
{-# DEPRECATED dcfContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

instance Lude.AWSRequest DescribeContactFlow where
  type Rs DescribeContactFlow = DescribeContactFlowResponse
  request = Req.get connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeContactFlowResponse'
            Lude.<$> (x Lude..?> "ContactFlow") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeContactFlow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeContactFlow where
  toPath DescribeContactFlow' {..} =
    Lude.mconcat
      [ "/contact-flows/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS contactFlowId
      ]

instance Lude.ToQuery DescribeContactFlow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeContactFlowResponse' smart constructor.
data DescribeContactFlowResponse = DescribeContactFlowResponse'
  { contactFlow ::
      Lude.Maybe ContactFlow,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeContactFlowResponse' with the minimum fields required to make a request.
--
-- * 'contactFlow' - Information about the contact flow.
-- * 'responseStatus' - The response status code.
mkDescribeContactFlowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeContactFlowResponse
mkDescribeContactFlowResponse pResponseStatus_ =
  DescribeContactFlowResponse'
    { contactFlow = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the contact flow.
--
-- /Note:/ Consider using 'contactFlow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrsContactFlow :: Lens.Lens' DescribeContactFlowResponse (Lude.Maybe ContactFlow)
dcfrsContactFlow = Lens.lens (contactFlow :: DescribeContactFlowResponse -> Lude.Maybe ContactFlow) (\s a -> s {contactFlow = a} :: DescribeContactFlowResponse)
{-# DEPRECATED dcfrsContactFlow "Use generic-lens or generic-optics with 'contactFlow' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfrsResponseStatus :: Lens.Lens' DescribeContactFlowResponse Lude.Int
dcfrsResponseStatus = Lens.lens (responseStatus :: DescribeContactFlowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeContactFlowResponse)
{-# DEPRECATED dcfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
