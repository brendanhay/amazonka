{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateProvisioningTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a fleet provisioning template.
module Network.AWS.IoT.CreateProvisioningTemplateVersion
  ( -- * Creating a request
    CreateProvisioningTemplateVersion (..),
    mkCreateProvisioningTemplateVersion,

    -- ** Request lenses
    cptvTemplateName,
    cptvSetAsDefault,
    cptvTemplateBody,

    -- * Destructuring the response
    CreateProvisioningTemplateVersionResponse (..),
    mkCreateProvisioningTemplateVersionResponse,

    -- ** Response lenses
    cptvrsVersionId,
    cptvrsTemplateName,
    cptvrsTemplateARN,
    cptvrsIsDefaultVersion,
    cptvrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateProvisioningTemplateVersion' smart constructor.
data CreateProvisioningTemplateVersion = CreateProvisioningTemplateVersion'
  { -- | The name of the fleet provisioning template.
    templateName :: Lude.Text,
    -- | Sets a fleet provision template version as the default version.
    setAsDefault :: Lude.Maybe Lude.Bool,
    -- | The JSON formatted contents of the fleet provisioning template.
    templateBody :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProvisioningTemplateVersion' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the fleet provisioning template.
-- * 'setAsDefault' - Sets a fleet provision template version as the default version.
-- * 'templateBody' - The JSON formatted contents of the fleet provisioning template.
mkCreateProvisioningTemplateVersion ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'templateBody'
  Lude.Text ->
  CreateProvisioningTemplateVersion
mkCreateProvisioningTemplateVersion pTemplateName_ pTemplateBody_ =
  CreateProvisioningTemplateVersion'
    { templateName = pTemplateName_,
      setAsDefault = Lude.Nothing,
      templateBody = pTemplateBody_
    }

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvTemplateName :: Lens.Lens' CreateProvisioningTemplateVersion Lude.Text
cptvTemplateName = Lens.lens (templateName :: CreateProvisioningTemplateVersion -> Lude.Text) (\s a -> s {templateName = a} :: CreateProvisioningTemplateVersion)
{-# DEPRECATED cptvTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | Sets a fleet provision template version as the default version.
--
-- /Note:/ Consider using 'setAsDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvSetAsDefault :: Lens.Lens' CreateProvisioningTemplateVersion (Lude.Maybe Lude.Bool)
cptvSetAsDefault = Lens.lens (setAsDefault :: CreateProvisioningTemplateVersion -> Lude.Maybe Lude.Bool) (\s a -> s {setAsDefault = a} :: CreateProvisioningTemplateVersion)
{-# DEPRECATED cptvSetAsDefault "Use generic-lens or generic-optics with 'setAsDefault' instead." #-}

-- | The JSON formatted contents of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvTemplateBody :: Lens.Lens' CreateProvisioningTemplateVersion Lude.Text
cptvTemplateBody = Lens.lens (templateBody :: CreateProvisioningTemplateVersion -> Lude.Text) (\s a -> s {templateBody = a} :: CreateProvisioningTemplateVersion)
{-# DEPRECATED cptvTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

instance Lude.AWSRequest CreateProvisioningTemplateVersion where
  type
    Rs CreateProvisioningTemplateVersion =
      CreateProvisioningTemplateVersionResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateProvisioningTemplateVersionResponse'
            Lude.<$> (x Lude..?> "versionId")
            Lude.<*> (x Lude..?> "templateName")
            Lude.<*> (x Lude..?> "templateArn")
            Lude.<*> (x Lude..?> "isDefaultVersion")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateProvisioningTemplateVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateProvisioningTemplateVersion where
  toJSON CreateProvisioningTemplateVersion' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("templateBody" Lude..= templateBody)])

instance Lude.ToPath CreateProvisioningTemplateVersion where
  toPath CreateProvisioningTemplateVersion' {..} =
    Lude.mconcat
      ["/provisioning-templates/", Lude.toBS templateName, "/versions"]

instance Lude.ToQuery CreateProvisioningTemplateVersion where
  toQuery CreateProvisioningTemplateVersion' {..} =
    Lude.mconcat ["setAsDefault" Lude.=: setAsDefault]

-- | /See:/ 'mkCreateProvisioningTemplateVersionResponse' smart constructor.
data CreateProvisioningTemplateVersionResponse = CreateProvisioningTemplateVersionResponse'
  { -- | The version of the fleet provisioning template.
    versionId :: Lude.Maybe Lude.Int,
    -- | The name of the fleet provisioning template.
    templateName :: Lude.Maybe Lude.Text,
    -- | The ARN that identifies the provisioning template.
    templateARN :: Lude.Maybe Lude.Text,
    -- | True if the fleet provisioning template version is the default version, otherwise false.
    isDefaultVersion :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateProvisioningTemplateVersionResponse' with the minimum fields required to make a request.
--
-- * 'versionId' - The version of the fleet provisioning template.
-- * 'templateName' - The name of the fleet provisioning template.
-- * 'templateARN' - The ARN that identifies the provisioning template.
-- * 'isDefaultVersion' - True if the fleet provisioning template version is the default version, otherwise false.
-- * 'responseStatus' - The response status code.
mkCreateProvisioningTemplateVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateProvisioningTemplateVersionResponse
mkCreateProvisioningTemplateVersionResponse pResponseStatus_ =
  CreateProvisioningTemplateVersionResponse'
    { versionId =
        Lude.Nothing,
      templateName = Lude.Nothing,
      templateARN = Lude.Nothing,
      isDefaultVersion = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The version of the fleet provisioning template.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvrsVersionId :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Lude.Maybe Lude.Int)
cptvrsVersionId = Lens.lens (versionId :: CreateProvisioningTemplateVersionResponse -> Lude.Maybe Lude.Int) (\s a -> s {versionId = a} :: CreateProvisioningTemplateVersionResponse)
{-# DEPRECATED cptvrsVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the fleet provisioning template.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvrsTemplateName :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Lude.Maybe Lude.Text)
cptvrsTemplateName = Lens.lens (templateName :: CreateProvisioningTemplateVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateName = a} :: CreateProvisioningTemplateVersionResponse)
{-# DEPRECATED cptvrsTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The ARN that identifies the provisioning template.
--
-- /Note:/ Consider using 'templateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvrsTemplateARN :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Lude.Maybe Lude.Text)
cptvrsTemplateARN = Lens.lens (templateARN :: CreateProvisioningTemplateVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateARN = a} :: CreateProvisioningTemplateVersionResponse)
{-# DEPRECATED cptvrsTemplateARN "Use generic-lens or generic-optics with 'templateARN' instead." #-}

-- | True if the fleet provisioning template version is the default version, otherwise false.
--
-- /Note:/ Consider using 'isDefaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvrsIsDefaultVersion :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Lude.Maybe Lude.Bool)
cptvrsIsDefaultVersion = Lens.lens (isDefaultVersion :: CreateProvisioningTemplateVersionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isDefaultVersion = a} :: CreateProvisioningTemplateVersionResponse)
{-# DEPRECATED cptvrsIsDefaultVersion "Use generic-lens or generic-optics with 'isDefaultVersion' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cptvrsResponseStatus :: Lens.Lens' CreateProvisioningTemplateVersionResponse Lude.Int
cptvrsResponseStatus = Lens.lens (responseStatus :: CreateProvisioningTemplateVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateProvisioningTemplateVersionResponse)
{-# DEPRECATED cptvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
