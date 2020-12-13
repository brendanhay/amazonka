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
    csaIdempotencyToken,
    csaDefinition,
    csaName,
    csaAcceptLanguage,
    csaDefinitionType,
    csaDescription,

    -- * Destructuring the response
    CreateServiceActionResponse (..),
    mkCreateServiceActionResponse,

    -- ** Response lenses
    csarsServiceActionDetail,
    csarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'mkCreateServiceAction' smart constructor.
data CreateServiceAction = CreateServiceAction'
  { -- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
    idempotencyToken :: Lude.Text,
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
    definition :: Lude.HashMap ServiceActionDefinitionKey (Lude.Text),
    -- | The self-service action name.
    name :: Lude.Text,
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
    acceptLanguage :: Lude.Maybe Lude.Text,
    -- | The service action definition type. For example, @SSM_AUTOMATION@ .
    definitionType :: ServiceActionDefinitionType,
    -- | The self-service action description.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateServiceAction' with the minimum fields required to make a request.
--
-- * 'idempotencyToken' - A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
-- * 'definition' - The self-service action definition. Can be one of the following:
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
-- * 'name' - The self-service action name.
-- * 'acceptLanguage' - The language code.
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
-- * 'definitionType' - The service action definition type. For example, @SSM_AUTOMATION@ .
-- * 'description' - The self-service action description.
mkCreateServiceAction ::
  -- | 'idempotencyToken'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'definitionType'
  ServiceActionDefinitionType ->
  CreateServiceAction
mkCreateServiceAction pIdempotencyToken_ pName_ pDefinitionType_ =
  CreateServiceAction'
    { idempotencyToken = pIdempotencyToken_,
      definition = Lude.mempty,
      name = pName_,
      acceptLanguage = Lude.Nothing,
      definitionType = pDefinitionType_,
      description = Lude.Nothing
    }

-- | A unique identifier that you provide to ensure idempotency. If multiple requests differ only by the idempotency token, the same response is returned for each repeated request.
--
-- /Note:/ Consider using 'idempotencyToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaIdempotencyToken :: Lens.Lens' CreateServiceAction Lude.Text
csaIdempotencyToken = Lens.lens (idempotencyToken :: CreateServiceAction -> Lude.Text) (\s a -> s {idempotencyToken = a} :: CreateServiceAction)
{-# DEPRECATED csaIdempotencyToken "Use generic-lens or generic-optics with 'idempotencyToken' instead." #-}

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
csaDefinition :: Lens.Lens' CreateServiceAction (Lude.HashMap ServiceActionDefinitionKey (Lude.Text))
csaDefinition = Lens.lens (definition :: CreateServiceAction -> Lude.HashMap ServiceActionDefinitionKey (Lude.Text)) (\s a -> s {definition = a} :: CreateServiceAction)
{-# DEPRECATED csaDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The self-service action name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaName :: Lens.Lens' CreateServiceAction Lude.Text
csaName = Lens.lens (name :: CreateServiceAction -> Lude.Text) (\s a -> s {name = a} :: CreateServiceAction)
{-# DEPRECATED csaName "Use generic-lens or generic-optics with 'name' instead." #-}

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
csaAcceptLanguage :: Lens.Lens' CreateServiceAction (Lude.Maybe Lude.Text)
csaAcceptLanguage = Lens.lens (acceptLanguage :: CreateServiceAction -> Lude.Maybe Lude.Text) (\s a -> s {acceptLanguage = a} :: CreateServiceAction)
{-# DEPRECATED csaAcceptLanguage "Use generic-lens or generic-optics with 'acceptLanguage' instead." #-}

-- | The service action definition type. For example, @SSM_AUTOMATION@ .
--
-- /Note:/ Consider using 'definitionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaDefinitionType :: Lens.Lens' CreateServiceAction ServiceActionDefinitionType
csaDefinitionType = Lens.lens (definitionType :: CreateServiceAction -> ServiceActionDefinitionType) (\s a -> s {definitionType = a} :: CreateServiceAction)
{-# DEPRECATED csaDefinitionType "Use generic-lens or generic-optics with 'definitionType' instead." #-}

-- | The self-service action description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csaDescription :: Lens.Lens' CreateServiceAction (Lude.Maybe Lude.Text)
csaDescription = Lens.lens (description :: CreateServiceAction -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateServiceAction)
{-# DEPRECATED csaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest CreateServiceAction where
  type Rs CreateServiceAction = CreateServiceActionResponse
  request = Req.postJSON serviceCatalogService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateServiceActionResponse'
            Lude.<$> (x Lude..?> "ServiceActionDetail")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateServiceAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWS242ServiceCatalogService.CreateServiceAction" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateServiceAction where
  toJSON CreateServiceAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IdempotencyToken" Lude..= idempotencyToken),
            Lude.Just ("Definition" Lude..= definition),
            Lude.Just ("Name" Lude..= name),
            ("AcceptLanguage" Lude..=) Lude.<$> acceptLanguage,
            Lude.Just ("DefinitionType" Lude..= definitionType),
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath CreateServiceAction where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateServiceAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateServiceActionResponse' smart constructor.
data CreateServiceActionResponse = CreateServiceActionResponse'
  { -- | An object containing information about the self-service action.
    serviceActionDetail :: Lude.Maybe ServiceActionDetail,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateServiceActionResponse' with the minimum fields required to make a request.
--
-- * 'serviceActionDetail' - An object containing information about the self-service action.
-- * 'responseStatus' - The response status code.
mkCreateServiceActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateServiceActionResponse
mkCreateServiceActionResponse pResponseStatus_ =
  CreateServiceActionResponse'
    { serviceActionDetail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An object containing information about the self-service action.
--
-- /Note:/ Consider using 'serviceActionDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csarsServiceActionDetail :: Lens.Lens' CreateServiceActionResponse (Lude.Maybe ServiceActionDetail)
csarsServiceActionDetail = Lens.lens (serviceActionDetail :: CreateServiceActionResponse -> Lude.Maybe ServiceActionDetail) (\s a -> s {serviceActionDetail = a} :: CreateServiceActionResponse)
{-# DEPRECATED csarsServiceActionDetail "Use generic-lens or generic-optics with 'serviceActionDetail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csarsResponseStatus :: Lens.Lens' CreateServiceActionResponse Lude.Int
csarsResponseStatus = Lens.lens (responseStatus :: CreateServiceActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateServiceActionResponse)
{-# DEPRECATED csarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
