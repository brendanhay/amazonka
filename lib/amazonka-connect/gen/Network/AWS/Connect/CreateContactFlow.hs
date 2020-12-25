{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    ccfInstanceId,
    ccfName,
    ccfType,
    ccfContent,
    ccfDescription,
    ccfTags,

    -- * Destructuring the response
    CreateContactFlowResponse (..),
    mkCreateContactFlowResponse,

    -- ** Response lenses
    ccfrrsContactFlowArn,
    ccfrrsContactFlowId,
    ccfrrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateContactFlow' smart constructor.
data CreateContactFlow = CreateContactFlow'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The name of the contact flow.
    name :: Types.Name,
    -- | The type of the contact flow. For descriptions of the available types, see <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type> in the /Amazon Connect Administrator Guide/ .
    type' :: Types.ContactFlowType,
    -- | The content of the contact flow.
    content :: Types.Content,
    -- | The description of the contact flow.
    description :: Core.Maybe Types.ContactFlowDescription,
    -- | One or more tags.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateContactFlow' value with any optional fields omitted.
mkCreateContactFlow ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'name'
  Types.Name ->
  -- | 'type\''
  Types.ContactFlowType ->
  -- | 'content'
  Types.Content ->
  CreateContactFlow
mkCreateContactFlow instanceId name type' content =
  CreateContactFlow'
    { instanceId,
      name,
      type',
      content,
      description = Core.Nothing,
      tags = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfInstanceId :: Lens.Lens' CreateContactFlow Types.InstanceId
ccfInstanceId = Lens.field @"instanceId"
{-# DEPRECATED ccfInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the contact flow.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfName :: Lens.Lens' CreateContactFlow Types.Name
ccfName = Lens.field @"name"
{-# DEPRECATED ccfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the contact flow. For descriptions of the available types, see <https://docs.aws.amazon.com/connect/latest/adminguide/create-contact-flow.html#contact-flow-types Choose a Contact Flow Type> in the /Amazon Connect Administrator Guide/ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfType :: Lens.Lens' CreateContactFlow Types.ContactFlowType
ccfType = Lens.field @"type'"
{-# DEPRECATED ccfType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The content of the contact flow.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfContent :: Lens.Lens' CreateContactFlow Types.Content
ccfContent = Lens.field @"content"
{-# DEPRECATED ccfContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The description of the contact flow.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfDescription :: Lens.Lens' CreateContactFlow (Core.Maybe Types.ContactFlowDescription)
ccfDescription = Lens.field @"description"
{-# DEPRECATED ccfDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | One or more tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfTags :: Lens.Lens' CreateContactFlow (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
ccfTags = Lens.field @"tags"
{-# DEPRECATED ccfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateContactFlow where
  toJSON CreateContactFlow {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Type" Core..= type'),
            Core.Just ("Content" Core..= content),
            ("Description" Core..=) Core.<$> description,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateContactFlow where
  type Rs CreateContactFlow = CreateContactFlowResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath ("/contact-flows/" Core.<> (Core.toText instanceId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContactFlowResponse'
            Core.<$> (x Core..:? "ContactFlowArn")
            Core.<*> (x Core..:? "ContactFlowId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateContactFlowResponse' smart constructor.
data CreateContactFlowResponse = CreateContactFlowResponse'
  { -- | The Amazon Resource Name (ARN) of the contact flow.
    contactFlowArn :: Core.Maybe Types.ARN,
    -- | The identifier of the contact flow.
    contactFlowId :: Core.Maybe Types.ContactFlowId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateContactFlowResponse' value with any optional fields omitted.
mkCreateContactFlowResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateContactFlowResponse
mkCreateContactFlowResponse responseStatus =
  CreateContactFlowResponse'
    { contactFlowArn = Core.Nothing,
      contactFlowId = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the contact flow.
--
-- /Note:/ Consider using 'contactFlowArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfrrsContactFlowArn :: Lens.Lens' CreateContactFlowResponse (Core.Maybe Types.ARN)
ccfrrsContactFlowArn = Lens.field @"contactFlowArn"
{-# DEPRECATED ccfrrsContactFlowArn "Use generic-lens or generic-optics with 'contactFlowArn' instead." #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'contactFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfrrsContactFlowId :: Lens.Lens' CreateContactFlowResponse (Core.Maybe Types.ContactFlowId)
ccfrrsContactFlowId = Lens.field @"contactFlowId"
{-# DEPRECATED ccfrrsContactFlowId "Use generic-lens or generic-optics with 'contactFlowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfrrsResponseStatus :: Lens.Lens' CreateContactFlowResponse Core.Int
ccfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
