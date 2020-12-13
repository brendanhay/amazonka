{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a fleet provisioning template.
module Network.AWS.IoT.UpdateProvisioningTemplate
  ( -- * Creating a request
    UpdateProvisioningTemplate (..),
    mkUpdateProvisioningTemplate,

    -- ** Request lenses
    uptTemplateName,
    uptPreProvisioningHook,
    uptEnabled,
    uptProvisioningRoleARN,
    uptDefaultVersionId,
    uptRemovePreProvisioningHook,
    uptDescription,

    -- * Destructuring the response
    UpdateProvisioningTemplateResponse (..),
    mkUpdateProvisioningTemplateResponse,

    -- ** Response lenses
    uptrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateProvisioningTemplate' smart constructor.
data UpdateProvisioningTemplate = UpdateProvisioningTemplate'
  { -- | The name of the fleet provisioning template.
    templateName :: Lude.Text,
    -- | Updates the pre-provisioning hook template.
    preProvisioningHook :: Lude.Maybe ProvisioningHook,
    -- | True to enable the fleet provisioning template, otherwise false.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
    provisioningRoleARN :: Lude.Maybe Lude.Text,
    -- | The ID of the default provisioning template version.
    defaultVersionId :: Lude.Maybe Lude.Int,
    -- | Removes pre-provisioning hook template.
    removePreProvisioningHook :: Lude.Maybe Lude.Bool,
    -- | The description of the fleet provisioning template.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProvisioningTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the fleet provisioning template.
-- * 'preProvisioningHook' - Updates the pre-provisioning hook template.
-- * 'enabled' - True to enable the fleet provisioning template, otherwise false.
-- * 'provisioningRoleARN' - The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
-- * 'defaultVersionId' - The ID of the default provisioning template version.
-- * 'removePreProvisioningHook' - Removes pre-provisioning hook template.
-- * 'description' - The description of the fleet provisioning template.
mkUpdateProvisioningTemplate ::
  -- | 'templateName'
  Lude.Text ->
  UpdateProvisioningTemplate
mkUpdateProvisioningTemplate pTemplateName_ =
  UpdateProvisioningTemplate'
    { templateName = pTemplateName_,
      preProvisioningHook = Lude.Nothing,
      enabled = Lude.Nothing,
      provisioningRoleARN = Lude.Nothing,
      defaultVersionId = Lude.Nothing,
      removePreProvisioningHook = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptTemplateName :: Lens.Lens' UpdateProvisioningTemplate Lude.Text
uptTemplateName = Lens.lens (templateName :: UpdateProvisioningTemplate -> Lude.Text) (\s a -> s {templateName = a} :: UpdateProvisioningTemplate)
{-# DEPRECATED uptTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Updates the pre-provisioning hook template.
--
-- /Note:/ Consider using 'preProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptPreProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Lude.Maybe ProvisioningHook)
uptPreProvisioningHook = Lens.lens (preProvisioningHook :: UpdateProvisioningTemplate -> Lude.Maybe ProvisioningHook) (\s a -> s {preProvisioningHook = a} :: UpdateProvisioningTemplate)
{-# DEPRECATED uptPreProvisioningHook "Use generic-lens or generic-optics with 'preProvisioningHook' instead." #-}

-- | True to enable the fleet provisioning template, otherwise false.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEnabled :: Lens.Lens' UpdateProvisioningTemplate (Lude.Maybe Lude.Bool)
uptEnabled = Lens.lens (enabled :: UpdateProvisioningTemplate -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: UpdateProvisioningTemplate)
{-# DEPRECATED uptEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The ARN of the role associated with the provisioning template. This IoT role grants permission to provision a device.
--
-- /Note:/ Consider using 'provisioningRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptProvisioningRoleARN :: Lens.Lens' UpdateProvisioningTemplate (Lude.Maybe Lude.Text)
uptProvisioningRoleARN = Lens.lens (provisioningRoleARN :: UpdateProvisioningTemplate -> Lude.Maybe Lude.Text) (\s a -> s {provisioningRoleARN = a} :: UpdateProvisioningTemplate)
{-# DEPRECATED uptProvisioningRoleARN "Use generic-lens or generic-optics with 'provisioningRoleARN' instead." #-}

-- | The ID of the default provisioning template version.
--
-- /Note:/ Consider using 'defaultVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptDefaultVersionId :: Lens.Lens' UpdateProvisioningTemplate (Lude.Maybe Lude.Int)
uptDefaultVersionId = Lens.lens (defaultVersionId :: UpdateProvisioningTemplate -> Lude.Maybe Lude.Int) (\s a -> s {defaultVersionId = a} :: UpdateProvisioningTemplate)
{-# DEPRECATED uptDefaultVersionId "Use generic-lens or generic-optics with 'defaultVersionId' instead." #-}

-- | Removes pre-provisioning hook template.
--
-- /Note:/ Consider using 'removePreProvisioningHook' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptRemovePreProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Lude.Maybe Lude.Bool)
uptRemovePreProvisioningHook = Lens.lens (removePreProvisioningHook :: UpdateProvisioningTemplate -> Lude.Maybe Lude.Bool) (\s a -> s {removePreProvisioningHook = a} :: UpdateProvisioningTemplate)
{-# DEPRECATED uptRemovePreProvisioningHook "Use generic-lens or generic-optics with 'removePreProvisioningHook' instead." #-}

-- | The description of the fleet provisioning template.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptDescription :: Lens.Lens' UpdateProvisioningTemplate (Lude.Maybe Lude.Text)
uptDescription = Lens.lens (description :: UpdateProvisioningTemplate -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateProvisioningTemplate)
{-# DEPRECATED uptDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateProvisioningTemplate where
  type
    Rs UpdateProvisioningTemplate =
      UpdateProvisioningTemplateResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateProvisioningTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateProvisioningTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateProvisioningTemplate where
  toJSON UpdateProvisioningTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("preProvisioningHook" Lude..=) Lude.<$> preProvisioningHook,
            ("enabled" Lude..=) Lude.<$> enabled,
            ("provisioningRoleArn" Lude..=) Lude.<$> provisioningRoleARN,
            ("defaultVersionId" Lude..=) Lude.<$> defaultVersionId,
            ("removePreProvisioningHook" Lude..=)
              Lude.<$> removePreProvisioningHook,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateProvisioningTemplate where
  toPath UpdateProvisioningTemplate' {..} =
    Lude.mconcat ["/provisioning-templates/", Lude.toBS templateName]

instance Lude.ToQuery UpdateProvisioningTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateProvisioningTemplateResponse' smart constructor.
newtype UpdateProvisioningTemplateResponse = UpdateProvisioningTemplateResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateProvisioningTemplateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateProvisioningTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateProvisioningTemplateResponse
mkUpdateProvisioningTemplateResponse pResponseStatus_ =
  UpdateProvisioningTemplateResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptrsResponseStatus :: Lens.Lens' UpdateProvisioningTemplateResponse Lude.Int
uptrsResponseStatus = Lens.lens (responseStatus :: UpdateProvisioningTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateProvisioningTemplateResponse)
{-# DEPRECATED uptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
