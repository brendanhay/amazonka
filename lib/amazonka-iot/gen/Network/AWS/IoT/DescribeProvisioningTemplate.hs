{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a fleet provisioning template.
module Network.AWS.IoT.DescribeProvisioningTemplate
  ( -- * Creating a request
    DescribeProvisioningTemplate (..),
    mkDescribeProvisioningTemplate,

    -- ** Request lenses
    dptTemplateName,

    -- * Destructuring the response
    DescribeProvisioningTemplateResponse (..),
    mkDescribeProvisioningTemplateResponse,

    -- ** Response lenses
    dptrsLastModifiedDate,
    dptrsTemplateName,
    dptrsPreProvisioningHook,
    dptrsEnabled,
    dptrsProvisioningRoleARN,
    dptrsDefaultVersionId,
    dptrsCreationDate,
    dptrsTemplateARN,
    dptrsTemplateBody,
    dptrsDescription,
    dptrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeProvisioningTemplate' smart constructor.
newtype DescribeProvisioningTemplate = DescribeProvisioningTemplate'
  { -- | The name of the fleet provisioning template.
    templateName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProvisioningTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the fleet provisioning template.
mkDescribeProvisioningTemplate ::
  -- | 'templateName'
  Lude.Text ->
  DescribeProvisioningTemplate
mkDescribeProvisioningTemplate pTemplateName_ =
  DescribeProvisioningTemplate' {templateName = pTemplateName_}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptTemplateName :: Lens.Lens' DescribeProvisioningTemplate Lude.Text
dptTemplateName = Lens.lens (templateName :: DescribeProvisioningTemplate -> Lude.Text) (\s a -> s {templateName = a} :: DescribeProvisioningTemplate)
{-# DEPRECATED dptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest DescribeProvisioningTemplate where
  type
    Rs DescribeProvisioningTemplate =
      DescribeProvisioningTemplateResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProvisioningTemplateResponse'
            Lude.<$> (x Lude..?> "lastModifiedDate")
            Lude.<*> (x Lude..?> "templateName")
            Lude.<*> (x Lude..?> "preProvisioningHook")
            Lude.<*> (x Lude..?> "enabled")
            Lude.<*> (x Lude..?> "provisioningRoleArn")
            Lude.<*> (x Lude..?> "defaultVersionId")
            Lude.<*> (x Lude..?> "creationDate")
            Lude.<*> (x Lude..?> "templateArn")
            Lude.<*> (x Lude..?> "templateBody")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeProvisioningTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeProvisioningTemplate where
  toPath DescribeProvisioningTemplate' {..} =
    Lude.mconcat ["/provisioning-templates/", Lude.toBS templateName]

instance Lude.ToQuery DescribeProvisioningTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProvisioningTemplateResponse' smart constructor.
data DescribeProvisioningTemplateResponse = DescribeProvisioningTemplateResponse'
  { -- | The date when the fleet provisioning template was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the fleet provisioning template.
    templateName :: Lude.Maybe Lude.Text,
    -- | Gets information about a pre-provisioned hook.
    preProvisioningHook :: Lude.Maybe ProvisioningHook,
    -- | True if the fleet provisioning template is enabled, otherwise false.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
    provisioningRoleARN :: Lude.Maybe Lude.Text,
    -- | The default fleet template version ID.
    defaultVersionId :: Lude.Maybe Lude.Int,
    -- | The date when the fleet provisioning template was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The ARN of the fleet provisioning template.
    templateARN :: Lude.Maybe Lude.Text,
    -- | The JSON formatted contents of the fleet provisioning template.
    templateBody :: Lude.Maybe Lude.Text,
    -- | The description of the fleet provisioning template.
    description :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProvisioningTemplateResponse' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - The date when the fleet provisioning template was last modified.
-- * 'templateName' - The name of the fleet provisioning template.
-- * 'preProvisioningHook' - Gets information about a pre-provisioned hook.
-- * 'enabled' - True if the fleet provisioning template is enabled, otherwise false.
-- * 'provisioningRoleARN' - The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
-- * 'defaultVersionId' - The default fleet template version ID.
-- * 'creationDate' - The date when the fleet provisioning template was created.
-- * 'templateARN' - The ARN of the fleet provisioning template.
-- * 'templateBody' - The JSON formatted contents of the fleet provisioning template.
-- * 'description' - The description of the fleet provisioning template.
-- * 'responseStatus' - The response status code.
mkDescribeProvisioningTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeProvisioningTemplateResponse
mkDescribeProvisioningTemplateResponse pResponseStatus_ =
  DescribeProvisioningTemplateResponse'
    { lastModifiedDate =
        Lude.Nothing,
      templateName = Lude.Nothing,
      preProvisioningHook = Lude.Nothing,
      enabled = Lude.Nothing,
      provisioningRoleARN = Lude.Nothing,
      defaultVersionId = Lude.Nothing,
      creationDate = Lude.Nothing,
      templateARN = Lude.Nothing,
      templateBody = Lude.Nothing,
      description = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date when the fleet provisioning template was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsLastModifiedDate :: Lens.Lens' DescribeProvisioningTemplateResponse (Lude.Maybe Lude.Timestamp)
dptrsLastModifiedDate = Lens.lens (lastModifiedDate :: DescribeProvisioningTemplateResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsTemplateName :: Lens.Lens' DescribeProvisioningTemplateResponse (Lude.Maybe Lude.Text)
dptrsTemplateName = Lens.lens (templateName :: DescribeProvisioningTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Gets information about a pre-provisioned hook.
--
-- /Note:/ Consider using 'preProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsPreProvisioningHook :: Lens.Lens' DescribeProvisioningTemplateResponse (Lude.Maybe ProvisioningHook)
dptrsPreProvisioningHook = Lens.lens (preProvisioningHook :: DescribeProvisioningTemplateResponse -> Lude.Maybe ProvisioningHook) (\s a -> s {preProvisioningHook = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsPreProvisioningHook "Use generic-lens or generic-optics with 'preProvisioningHook' instead." #-}

-- | True if the fleet provisioning template is enabled, otherwise false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsEnabled :: Lens.Lens' DescribeProvisioningTemplateResponse (Lude.Maybe Lude.Bool)
dptrsEnabled = Lens.lens (enabled :: DescribeProvisioningTemplateResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
--
-- /Note:/ Consider using 'provisioningRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsProvisioningRoleARN :: Lens.Lens' DescribeProvisioningTemplateResponse (Lude.Maybe Lude.Text)
dptrsProvisioningRoleARN = Lens.lens (provisioningRoleARN :: DescribeProvisioningTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {provisioningRoleARN = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsProvisioningRoleARN "Use generic-lens or generic-optics with 'provisioningRoleARN' instead." #-}

-- | The default fleet template version ID.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsDefaultVersionId :: Lens.Lens' DescribeProvisioningTemplateResponse (Lude.Maybe Lude.Int)
dptrsDefaultVersionId = Lens.lens (defaultVersionId :: DescribeProvisioningTemplateResponse -> Lude.Maybe Lude.Int) (\s a -> s {defaultVersionId = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | The date when the fleet provisioning template was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsCreationDate :: Lens.Lens' DescribeProvisioningTemplateResponse (Lude.Maybe Lude.Timestamp)
dptrsCreationDate = Lens.lens (creationDate :: DescribeProvisioningTemplateResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The ARN of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsTemplateARN :: Lens.Lens' DescribeProvisioningTemplateResponse (Lude.Maybe Lude.Text)
dptrsTemplateARN = Lens.lens (templateARN :: DescribeProvisioningTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateARN = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsTemplateARN "Use generic-lens or generic-optics with 'templateARN' instead." #-}

-- | The JSON formatted contents of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsTemplateBody :: Lens.Lens' DescribeProvisioningTemplateResponse (Lude.Maybe Lude.Text)
dptrsTemplateBody = Lens.lens (templateBody :: DescribeProvisioningTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | The description of the fleet provisioning template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsDescription :: Lens.Lens' DescribeProvisioningTemplateResponse (Lude.Maybe Lude.Text)
dptrsDescription = Lens.lens (description :: DescribeProvisioningTemplateResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsResponseStatus :: Lens.Lens' DescribeProvisioningTemplateResponse Lude.Int
dptrsResponseStatus = Lens.lens (responseStatus :: DescribeProvisioningTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProvisioningTemplateResponse)
{-# DEPRECATED dptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
