{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.CreateServiceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a self-service action.
module Network.AWS.ServiceCatalog.CreateServiceAction
  ( -- * Creating a request
    CreateServiceAction (..),
    mkCreateServiceAction,

    -- ** Request lenses
    csaName,
    csaDefinitionType,
    csaDefinition,
    csaIdempotencyToken,
    csaAcceptLanguage,
    csaDescription,

    -- * Destructuring the response
    CreateServiceActionResponse (..),
    mkCreateServiceActionResponse,

    -- ** Response lenses
    csarrsServiceActionDetail,
    csarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServiceCatalog.Types as Types

-- | /See:/ 'mkCreateServiceAction' smart constructor.
data CreateServiceAction = CreateServiceAction'
  { -- | The self-service action name.
    name :: Types.ServiceActionName,
    -- | The service action definition type. For example, @SSM_AUTOMATION@ .
    definitionType :: Types.ServiceActionDefinitionType,
    -- | The self-service action definition. Can be one of the following:
    --
    --
    --     * Name
    --
    --     * The name of the AWS Systems Manager document (SSM document). For example, @AWS-RestartEC2Instance@ .
    -- If you are using a shared SSM document, you must provide the ARN instead of the name.
    --
    --
    --     * Version
    --
    --     * The AWS Systems Manager automation document version. For example, @"Version": "1"@
    --
    --
    --     * AssumeRole
    --
    --     * The Amazon Resource Name (ARN) of the role that performs the self-service actions on your behalf. For example, @"AssumeRole": "arn:aws:iam::12345678910:role/ActionRole"@ .
    -- To reuse the provisioned product launch role, set to @"AssumeRole": "LAUNCH_ROLE"@ .
    --
    --
    --     * Parameters
    --
    --     * The list of parameters in JSON format.
    -- For example: @[{\"Name\":\"InstanceId\",\"Type\":\"TARGET\"}]@ or @[{\"Name\":\"InstanceId\",\"Type\":\"TEXT_VALUE\"}]@ .
    definition :: Core.HashMap Types.ServiceActionDefinitionKey Types.ServiceActionDefinitionValue,
    -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Types.IdempotencyToken,
    -- | The language code.
    --
    --
    --     * @en@ - English (default)
    --
    --
    --     * @jp@ - Japanese
    --
    --
    --     * @zh@ - Chinese
    acceptLanguage :: Core.Maybe Types.AcceptLanguage,
    -- | The self-service action description.
    description :: Core.Maybe Types.ServiceActionDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateServiceAction' value with any optional fields omitted.
mkCreateServiceAction ::
  -- | 'name'
  Types.ServiceActionName ->
  -- | 'definitionType'
  Types.ServiceActionDefinitionType ->
  -- | 'idempotencyToken'
  Types.IdempotencyToken ->
  CreateServiceAction
mkCreateServiceAction name definitionType idempotencyToken =
  CreateServiceAction'
    { name,
      definitionType,
      definition = Core.mempty,
      idempotencyToken,
      acceptLanguage = Core.Nothing,
      description = Core.Nothing
    }

-- | The self-service action name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaName :: Lens.Lens' CreateServiceAction Types.ServiceActionName
csaName = Lens.field @"name"
{-# DEPRECATED csaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The service action definition type. For example, @SSM_AUTOMATION@ .
--
-- /Note:/ Consider using 'definitionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaDefinitionType :: Lens.Lens' CreateServiceAction Types.ServiceActionDefinitionType
csaDefinitionType = Lens.field @"definitionType"
{-# DEPRECATED csaDefinitionType "Use generic-lens or generic-optics with 'definitionType' instead." #-}

-- | The self-service action definition. Can be one of the following:
--
--
--     * Name
--
--     * The name of the AWS Systems Manager document (SSM document). For example, @AWS-RestartEC2Instance@ .
-- If you are using a shared SSM document, you must provide the ARN instead of the name.
--
--
--     * Version
--
--     * The AWS Systems Manager automation document version. For example, @"Version": "1"@
--
--
--     * AssumeRole
--
--     * The Amazon Resource Name (ARN) of the role that performs the self-service actions on your behalf. For example, @"AssumeRole": "arn:aws:iam::12345678910:role/ActionRole"@ .
-- To reuse the provisioned product launch role, set to @"AssumeRole": "LAUNCH_ROLE"@ .
--
--
--     * Parameters
--
--     * The list of parameters in JSON format.
-- For example: @[{\"Name\":\"InstanceId\",\"Type\":\"TARGET\"}]@ or @[{\"Name\":\"InstanceId\",\"Type\":\"TEXT_VALUE\"}]@ .
--
--
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaDefinition :: Lens.Lens' CreateServiceAction (Core.HashMap Types.ServiceActionDefinitionKey Types.ServiceActionDefinitionValue)
csaDefinition = Lens.field @"definition"
{-# DEPRECATED csaDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaIdempotencyToken :: Lens.Lens' CreateServiceAction Types.IdempotencyToken
csaIdempotencyToken = Lens.field @"idempotencyToken"
{-# DEPRECATED csaIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

-- | The language code.
--
--
--     * @en@ - English (default)
--
--
--     * @jp@ - Japanese
--
--
--     * @zh@ - Chinese
--
--
--
-- /Note:/ Consider using 'acceptLanguage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaAcceptLanguage :: Lens.Lens' CreateServiceAction (Core.Maybe Types.AcceptLanguage)
csaAcceptLanguage = Lens.field @"acceptLanguage"
{-# DEPRECATED csaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The self-service action description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaDescription :: Lens.Lens' CreateServiceAction (Core.Maybe Types.ServiceActionDescription)
csaDescription = Lens.field @"description"
{-# DEPRECATED csaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON CreateServiceAction where
  toJSON CreateServiceAction {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("DefinitionType" Core..= definitionType),
            Core.Just ("Definition" Core..= definition),
            Core.Just ("IdempotencyToken" Core..= idempotencyToken),
            ("AcceptLanguage" Core..=) Core.<$> acceptLanguage,
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.AWSRequest CreateServiceAction where
  type Rs CreateServiceAction = CreateServiceActionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWS242ServiceCatalogService.CreateServiceAction")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceActionResponse'
            Core.<$> (x Core..:? "ServiceActionDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateServiceActionResponse' smart constructor.
data CreateServiceActionResponse = CreateServiceActionResponse'
  { -- | An object containing information about the self-service action.
    serviceActionDetail :: Core.Maybe Types.ServiceActionDetail,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateServiceActionResponse' value with any optional fields omitted.
mkCreateServiceActionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateServiceActionResponse
mkCreateServiceActionResponse responseStatus =
  CreateServiceActionResponse'
    { serviceActionDetail = Core.Nothing,
      responseStatus
    }

-- | An object containing information about the self-service action.
--
-- /Note:/ Consider using 'serviceActionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csarrsServiceActionDetail :: Lens.Lens' CreateServiceActionResponse (Core.Maybe Types.ServiceActionDetail)
csarrsServiceActionDetail = Lens.field @"serviceActionDetail"
{-# DEPRECATED csarrsServiceActionDetail "Use generic-lens or generic-optics with 'serviceActionDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csarrsResponseStatus :: Lens.Lens' CreateServiceActionResponse Core.Int
csarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
