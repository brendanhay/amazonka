{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet provisioning template.
module Network.AWS.IoT.CreateProvisioningTemplate
  ( -- * Creating a request
    CreateProvisioningTemplate (..),
    mkCreateProvisioningTemplate,

    -- ** Request lenses
    cptTemplateName,
    cptPreProvisioningHook,
    cptEnabled,
    cptProvisioningRoleARN,
    cptTemplateBody,
    cptDescription,
    cptTags,

    -- * Destructuring the response
    CreateProvisioningTemplateResponse (..),
    mkCreateProvisioningTemplateResponse,

    -- ** Response lenses
    cptrsTemplateName,
    cptrsDefaultVersionId,
    cptrsTemplateARN,
    cptrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateProvisioningTemplate' smart constructor.
data CreateProvisioningTemplate = CreateProvisioningTemplate'
  { -- | The name of the fleet provisioning template.
    templateName :: Lude.Text,
    -- | Creates a pre-provisioning hook template.
    preProvisioningHook :: Lude.Maybe ProvisioningHook,
    -- | True to enable the fleet provisioning template, otherwise false.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The role ARN for the role associated with the fleet provisioning template. This IoT role grants permission to provision a device.
    provisioningRoleARN :: Lude.Text,
    -- | The JSON formatted contents of the fleet provisioning template.
    templateBody :: Lude.Text,
    -- | The description of the fleet provisioning template.
    description :: Lude.Maybe Lude.Text,
    -- | Metadata which can be used to manage the fleet provisioning template.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProvisioningTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the fleet provisioning template.
-- * 'preProvisioningHook' - Creates a pre-provisioning hook template.
-- * 'enabled' - True to enable the fleet provisioning template, otherwise false.
-- * 'provisioningRoleARN' - The role ARN for the role associated with the fleet provisioning template. This IoT role grants permission to provision a device.
-- * 'templateBody' - The JSON formatted contents of the fleet provisioning template.
-- * 'description' - The description of the fleet provisioning template.
-- * 'tags' - Metadata which can be used to manage the fleet provisioning template.
mkCreateProvisioningTemplate ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'provisioningRoleARN'
  Lude.Text ->
  -- | 'templateBody'
  Lude.Text ->
  CreateProvisioningTemplate
mkCreateProvisioningTemplate
  pTemplateName_
  pProvisioningRoleARN_
  pTemplateBody_ =
    CreateProvisioningTemplate'
      { templateName = pTemplateName_,
        preProvisioningHook = Lude.Nothing,
        enabled = Lude.Nothing,
        provisioningRoleARN = pProvisioningRoleARN_,
        templateBody = pTemplateBody_,
        description = Lude.Nothing,
        tags = Lude.Nothing
      }

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptTemplateName :: Lens.Lens' CreateProvisioningTemplate Lude.Text
cptTemplateName = Lens.lens (templateName :: CreateProvisioningTemplate -> Lude.Text) (\s a -> s {templateName = a} :: CreateProvisioningTemplate)
{-# DEPRECATED cptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Creates a pre-provisioning hook template.
--
-- /Note:/ Consider using 'preProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptPreProvisioningHook :: Lens.Lens' CreateProvisioningTemplate (Lude.Maybe ProvisioningHook)
cptPreProvisioningHook = Lens.lens (preProvisioningHook :: CreateProvisioningTemplate -> Lude.Maybe ProvisioningHook) (\s a -> s {preProvisioningHook = a} :: CreateProvisioningTemplate)
{-# DEPRECATED cptPreProvisioningHook "Use generic-lens or generic-optics with 'preProvisioningHook' instead." #-}

-- | True to enable the fleet provisioning template, otherwise false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptEnabled :: Lens.Lens' CreateProvisioningTemplate (Lude.Maybe Lude.Bool)
cptEnabled = Lens.lens (enabled :: CreateProvisioningTemplate -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: CreateProvisioningTemplate)
{-# DEPRECATED cptEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The role ARN for the role associated with the fleet provisioning template. This IoT role grants permission to provision a device.
--
-- /Note:/ Consider using 'provisioningRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptProvisioningRoleARN :: Lens.Lens' CreateProvisioningTemplate Lude.Text
cptProvisioningRoleARN = Lens.lens (provisioningRoleARN :: CreateProvisioningTemplate -> Lude.Text) (\s a -> s {provisioningRoleARN = a} :: CreateProvisioningTemplate)
{-# DEPRECATED cptProvisioningRoleARN "Use generic-lens or generic-optics with 'provisioningRoleARN' instead." #-}

-- | The JSON formatted contents of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptTemplateBody :: Lens.Lens' CreateProvisioningTemplate Lude.Text
cptTemplateBody = Lens.lens (templateBody :: CreateProvisioningTemplate -> Lude.Text) (\s a -> s {templateBody = a} :: CreateProvisioningTemplate)
{-# DEPRECATED cptTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The description of the fleet provisioning template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptDescription :: Lens.Lens' CreateProvisioningTemplate (Lude.Maybe Lude.Text)
cptDescription = Lens.lens (description :: CreateProvisioningTemplate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateProvisioningTemplate)
{-# DEPRECATED cptDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Metadata which can be used to manage the fleet provisioning template.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptTags :: Lens.Lens' CreateProvisioningTemplate (Lude.Maybe [Tag])
cptTags = Lens.lens (tags :: CreateProvisioningTemplate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateProvisioningTemplate)
{-# DEPRECATED cptTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateProvisioningTemplate where
  type
    Rs CreateProvisioningTemplate =
      CreateProvisioningTemplateResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProvisioningTemplateResponse'
            Lude.<$> (x Lude..?> "templateName")
            Lude.<*> (x Lude..?> "defaultVersionId")
            Lude.<*> (x Lude..?> "templateArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProvisioningTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateProvisioningTemplate where
  toJSON CreateProvisioningTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("templateName" Lude..= templateName),
            ("preProvisioningHook" Lude..=) Lude.<$> preProvisioningHook,
            ("enabled" Lude..=) Lude.<$> enabled,
            Lude.Just ("provisioningRoleArn" Lude..= provisioningRoleARN),
            Lude.Just ("templateBody" Lude..= templateBody),
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateProvisioningTemplate where
  toPath = Lude.const "/provisioning-templates"

instance Lude.ToQuery CreateProvisioningTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateProvisioningTemplateResponse' smart constructor.
data CreateProvisioningTemplateResponse = CreateProvisioningTemplateResponse'
  { -- | The name of the fleet provisioning template.
    templateName :: Lude.Maybe Lude.Text,
    -- | The default version of the fleet provisioning template.
    defaultVersionId :: Lude.Maybe Lude.Int,
    -- | The ARN that identifies the provisioning template.
    templateARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProvisioningTemplateResponse' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the fleet provisioning template.
-- * 'defaultVersionId' - The default version of the fleet provisioning template.
-- * 'templateARN' - The ARN that identifies the provisioning template.
-- * 'responseStatus' - The response status code.
mkCreateProvisioningTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProvisioningTemplateResponse
mkCreateProvisioningTemplateResponse pResponseStatus_ =
  CreateProvisioningTemplateResponse'
    { templateName = Lude.Nothing,
      defaultVersionId = Lude.Nothing,
      templateARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrsTemplateName :: Lens.Lens' CreateProvisioningTemplateResponse (Lude.Maybe Lude.Text)
cptrsTemplateName = Lens.lens (templateName :: CreateProvisioningTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: CreateProvisioningTemplateResponse)
{-# DEPRECATED cptrsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The default version of the fleet provisioning template.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrsDefaultVersionId :: Lens.Lens' CreateProvisioningTemplateResponse (Lude.Maybe Lude.Int)
cptrsDefaultVersionId = Lens.lens (defaultVersionId :: CreateProvisioningTemplateResponse -> Lude.Maybe Lude.Int) (\s a -> s {defaultVersionId = a} :: CreateProvisioningTemplateResponse)
{-# DEPRECATED cptrsDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The ARN that identifies the provisioning template.
--
-- /Note:/ Consider using 'templateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrsTemplateARN :: Lens.Lens' CreateProvisioningTemplateResponse (Lude.Maybe Lude.Text)
cptrsTemplateARN = Lens.lens (templateARN :: CreateProvisioningTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateARN = a} :: CreateProvisioningTemplateResponse)
{-# DEPRECATED cptrsTemplateARN "Use generic-lens or generic-optics with 'templateARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptrsResponseStatus :: Lens.Lens' CreateProvisioningTemplateResponse Lude.Int
cptrsResponseStatus = Lens.lens (responseStatus :: CreateProvisioningTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProvisioningTemplateResponse)
{-# DEPRECATED cptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
