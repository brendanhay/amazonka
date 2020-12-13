{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.GetModelTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a sample mapping template that can be used to transform a payload into the structure of a model.
module Network.AWS.APIGateway.GetModelTemplate
  ( -- * Creating a request
    GetModelTemplate (..),
    mkGetModelTemplate,

    -- ** Request lenses
    gmtModelName,
    gmtRestAPIId,

    -- * Destructuring the response
    GetModelTemplateResponse (..),
    mkGetModelTemplateResponse,

    -- ** Response lenses
    gmtrsValue,
    gmtrsResponseStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to generate a sample mapping template used to transform the payload.
--
-- /See:/ 'mkGetModelTemplate' smart constructor.
data GetModelTemplate = GetModelTemplate'
  { -- | [Required] The name of the model for which to generate a template.
    modelName :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetModelTemplate' with the minimum fields required to make a request.
--
-- * 'modelName' - [Required] The name of the model for which to generate a template.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
mkGetModelTemplate ::
  -- | 'modelName'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  GetModelTemplate
mkGetModelTemplate pModelName_ pRestAPIId_ =
  GetModelTemplate'
    { modelName = pModelName_,
      restAPIId = pRestAPIId_
    }

-- | [Required] The name of the model for which to generate a template.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtModelName :: Lens.Lens' GetModelTemplate Lude.Text
gmtModelName = Lens.lens (modelName :: GetModelTemplate -> Lude.Text) (\s a -> s {modelName = a} :: GetModelTemplate)
{-# DEPRECATED gmtModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtRestAPIId :: Lens.Lens' GetModelTemplate Lude.Text
gmtRestAPIId = Lens.lens (restAPIId :: GetModelTemplate -> Lude.Text) (\s a -> s {restAPIId = a} :: GetModelTemplate)
{-# DEPRECATED gmtRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.AWSRequest GetModelTemplate where
  type Rs GetModelTemplate = GetModelTemplateResponse
  request = Req.get apiGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetModelTemplateResponse'
            Lude.<$> (x Lude..?> "value") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetModelTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToPath GetModelTemplate where
  toPath GetModelTemplate' {..} =
    Lude.mconcat
      [ "/restapis/",
        Lude.toBS restAPIId,
        "/models/",
        Lude.toBS modelName,
        "/default_template"
      ]

instance Lude.ToQuery GetModelTemplate where
  toQuery = Lude.const Lude.mempty

-- | Represents a mapping template used to transform a payload.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/models-mappings.html#models-mappings-mappings Mapping Templates>
--
-- /See:/ 'mkGetModelTemplateResponse' smart constructor.
data GetModelTemplateResponse = GetModelTemplateResponse'
  { -- | The Apache <https://velocity.apache.org/engine/devel/vtl-reference.html Velocity Template Language (VTL)> template content used for the template resource.
    value :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetModelTemplateResponse' with the minimum fields required to make a request.
--
-- * 'value' - The Apache <https://velocity.apache.org/engine/devel/vtl-reference.html Velocity Template Language (VTL)> template content used for the template resource.
-- * 'responseStatus' - The response status code.
mkGetModelTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetModelTemplateResponse
mkGetModelTemplateResponse pResponseStatus_ =
  GetModelTemplateResponse'
    { value = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Apache <https://velocity.apache.org/engine/devel/vtl-reference.html Velocity Template Language (VTL)> template content used for the template resource.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtrsValue :: Lens.Lens' GetModelTemplateResponse (Lude.Maybe Lude.Text)
gmtrsValue = Lens.lens (value :: GetModelTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: GetModelTemplateResponse)
{-# DEPRECATED gmtrsValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmtrsResponseStatus :: Lens.Lens' GetModelTemplateResponse Lude.Int
gmtrsResponseStatus = Lens.lens (responseStatus :: GetModelTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetModelTemplateResponse)
{-# DEPRECATED gmtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
