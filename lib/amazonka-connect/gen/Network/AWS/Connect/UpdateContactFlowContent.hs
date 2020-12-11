{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateContactFlowContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified contact flow.
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
module Network.AWS.Connect.UpdateContactFlowContent
  ( -- * Creating a request
    UpdateContactFlowContent (..),
    mkUpdateContactFlowContent,

    -- ** Request lenses
    ucfcInstanceId,
    ucfcContactFlowId,
    ucfcContent,

    -- * Destructuring the response
    UpdateContactFlowContentResponse (..),
    mkUpdateContactFlowContentResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateContactFlowContent' smart constructor.
data UpdateContactFlowContent = UpdateContactFlowContent'
  { instanceId ::
      Lude.Text,
    contactFlowId :: Lude.Text,
    content :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContactFlowContent' with the minimum fields required to make a request.
--
-- * 'contactFlowId' - The identifier of the contact flow.
-- * 'content' - The JSON string that represents contact flow’s content. For an example, see <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language-example.html Example contact flow in Amazon Connect Flow language> in the /Amazon Connect Administrator Guide/ .
-- * 'instanceId' - The identifier of the Amazon Connect instance.
mkUpdateContactFlowContent ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'contactFlowId'
  Lude.Text ->
  -- | 'content'
  Lude.Text ->
  UpdateContactFlowContent
mkUpdateContactFlowContent pInstanceId_ pContactFlowId_ pContent_ =
  UpdateContactFlowContent'
    { instanceId = pInstanceId_,
      contactFlowId = pContactFlowId_,
      content = pContent_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfcInstanceId :: Lens.Lens' UpdateContactFlowContent Lude.Text
ucfcInstanceId = Lens.lens (instanceId :: UpdateContactFlowContent -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateContactFlowContent)
{-# DEPRECATED ucfcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfcContactFlowId :: Lens.Lens' UpdateContactFlowContent Lude.Text
ucfcContactFlowId = Lens.lens (contactFlowId :: UpdateContactFlowContent -> Lude.Text) (\s a -> s {contactFlowId = a} :: UpdateContactFlowContent)
{-# DEPRECATED ucfcContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

-- | The JSON string that represents contact flow’s content. For an example, see <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language-example.html Example contact flow in Amazon Connect Flow language> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucfcContent :: Lens.Lens' UpdateContactFlowContent Lude.Text
ucfcContent = Lens.lens (content :: UpdateContactFlowContent -> Lude.Text) (\s a -> s {content = a} :: UpdateContactFlowContent)
{-# DEPRECATED ucfcContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Lude.AWSRequest UpdateContactFlowContent where
  type Rs UpdateContactFlowContent = UpdateContactFlowContentResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateContactFlowContentResponse'

instance Lude.ToHeaders UpdateContactFlowContent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateContactFlowContent where
  toJSON UpdateContactFlowContent' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Content" Lude..= content)])

instance Lude.ToPath UpdateContactFlowContent where
  toPath UpdateContactFlowContent' {..} =
    Lude.mconcat
      [ "/contact-flows/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS contactFlowId,
        "/content"
      ]

instance Lude.ToQuery UpdateContactFlowContent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateContactFlowContentResponse' smart constructor.
data UpdateContactFlowContentResponse = UpdateContactFlowContentResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContactFlowContentResponse' with the minimum fields required to make a request.
mkUpdateContactFlowContentResponse ::
  UpdateContactFlowContentResponse
mkUpdateContactFlowContentResponse =
  UpdateContactFlowContentResponse'
