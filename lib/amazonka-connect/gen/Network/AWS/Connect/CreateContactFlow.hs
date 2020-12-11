{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.CreateContactFlow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a contact flow for the specified Amazon Connect instance.
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
module Network.AWS.Connect.CreateContactFlow
  ( -- * Creating a request
    CreateContactFlow (..),
    mkCreateContactFlow,

    -- ** Request lenses
    ccfDescription,
    ccfTags,
    ccfInstanceId,
    ccfName,
    ccfType,
    ccfContent,

    -- * Destructuring the response
    CreateContactFlowResponse (..),
    mkCreateContactFlowResponse,

    -- ** Response lenses
    ccfrsContactFlowARN,
    ccfrsContactFlowId,
    ccfrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateContactFlow' smart constructor.
data CreateContactFlow = CreateContactFlow'
  { description ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    instanceId :: Lude.Text,
    name :: Lude.Text,
    type' :: ContactFlowType,
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

-- | Creates a value of 'CreateContactFlow' with the minimum fields required to make a request.
--
-- * 'content' - The content of the contact flow.
-- * 'description' - The description of the contact flow.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'name' - The name of the contact flow.
-- * 'tags' - One or more tags.
-- * 'type'' - The type of the contact flow. For descriptions of the available types, see <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type> in the /Amazon Connect Administrator Guide/ .
mkCreateContactFlow ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  ContactFlowType ->
  -- | 'content'
  Lude.Text ->
  CreateContactFlow
mkCreateContactFlow pInstanceId_ pName_ pType_ pContent_ =
  CreateContactFlow'
    { description = Lude.Nothing,
      tags = Lude.Nothing,
      instanceId = pInstanceId_,
      name = pName_,
      type' = pType_,
      content = pContent_
    }

-- | The description of the contact flow.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDescription :: Lens.Lens' CreateContactFlow (Lude.Maybe Lude.Text)
ccfDescription = Lens.lens (description :: CreateContactFlow -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateContactFlow)
{-# DEPRECATED ccfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTags :: Lens.Lens' CreateContactFlow (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ccfTags = Lens.lens (tags :: CreateContactFlow -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateContactFlow)
{-# DEPRECATED ccfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfInstanceId :: Lens.Lens' CreateContactFlow Lude.Text
ccfInstanceId = Lens.lens (instanceId :: CreateContactFlow -> Lude.Text) (\s a -> s {instanceId = a} :: CreateContactFlow)
{-# DEPRECATED ccfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the contact flow.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfName :: Lens.Lens' CreateContactFlow Lude.Text
ccfName = Lens.lens (name :: CreateContactFlow -> Lude.Text) (\s a -> s {name = a} :: CreateContactFlow)
{-# DEPRECATED ccfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the contact flow. For descriptions of the available types, see <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfType :: Lens.Lens' CreateContactFlow ContactFlowType
ccfType = Lens.lens (type' :: CreateContactFlow -> ContactFlowType) (\s a -> s {type' = a} :: CreateContactFlow)
{-# DEPRECATED ccfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The content of the contact flow.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfContent :: Lens.Lens' CreateContactFlow Lude.Text
ccfContent = Lens.lens (content :: CreateContactFlow -> Lude.Text) (\s a -> s {content = a} :: CreateContactFlow)
{-# DEPRECATED ccfContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Lude.AWSRequest CreateContactFlow where
  type Rs CreateContactFlow = CreateContactFlowResponse
  request = Req.putJSON connectService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateContactFlowResponse'
            Lude.<$> (x Lude..?> "ContactFlowArn")
            Lude.<*> (x Lude..?> "ContactFlowId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateContactFlow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateContactFlow where
  toJSON CreateContactFlow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Type" Lude..= type'),
            Lude.Just ("Content" Lude..= content)
          ]
      )

instance Lude.ToPath CreateContactFlow where
  toPath CreateContactFlow' {..} =
    Lude.mconcat ["/contact-flows/", Lude.toBS instanceId]

instance Lude.ToQuery CreateContactFlow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateContactFlowResponse' smart constructor.
data CreateContactFlowResponse = CreateContactFlowResponse'
  { contactFlowARN ::
      Lude.Maybe Lude.Text,
    contactFlowId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateContactFlowResponse' with the minimum fields required to make a request.
--
-- * 'contactFlowARN' - The Amazon Resource Name (ARN) of the contact flow.
-- * 'contactFlowId' - The identifier of the contact flow.
-- * 'responseStatus' - The response status code.
mkCreateContactFlowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateContactFlowResponse
mkCreateContactFlowResponse pResponseStatus_ =
  CreateContactFlowResponse'
    { contactFlowARN = Lude.Nothing,
      contactFlowId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the contact flow.
--
-- /Note:/ Consider using 'contactFlowARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfrsContactFlowARN :: Lens.Lens' CreateContactFlowResponse (Lude.Maybe Lude.Text)
ccfrsContactFlowARN = Lens.lens (contactFlowARN :: CreateContactFlowResponse -> Lude.Maybe Lude.Text) (\s a -> s {contactFlowARN = a} :: CreateContactFlowResponse)
{-# DEPRECATED ccfrsContactFlowARN "Use generic-lens or generic-optics with 'contactFlowARN' instead." #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfrsContactFlowId :: Lens.Lens' CreateContactFlowResponse (Lude.Maybe Lude.Text)
ccfrsContactFlowId = Lens.lens (contactFlowId :: CreateContactFlowResponse -> Lude.Maybe Lude.Text) (\s a -> s {contactFlowId = a} :: CreateContactFlowResponse)
{-# DEPRECATED ccfrsContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfrsResponseStatus :: Lens.Lens' CreateContactFlowResponse Lude.Int
ccfrsResponseStatus = Lens.lens (responseStatus :: CreateContactFlowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateContactFlowResponse)
{-# DEPRECATED ccfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
