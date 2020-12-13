{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteProvisioningTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet provisioning template version.
module Network.AWS.IoT.DeleteProvisioningTemplateVersion
  ( -- * Creating a request
    DeleteProvisioningTemplateVersion (..),
    mkDeleteProvisioningTemplateVersion,

    -- ** Request lenses
    dVersionId,
    dTemplateName,

    -- * Destructuring the response
    DeleteProvisioningTemplateVersionResponse (..),
    mkDeleteProvisioningTemplateVersionResponse,

    -- ** Response lenses
    dptvfrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProvisioningTemplateVersion' smart constructor.
data DeleteProvisioningTemplateVersion = DeleteProvisioningTemplateVersion'
  { -- | The fleet provisioning template version ID to delete.
    versionId :: Lude.Int,
    -- | The name of the fleet provisioning template version to delete.
    templateName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProvisioningTemplateVersion' with the minimum fields required to make a request.
--
-- * 'versionId' - The fleet provisioning template version ID to delete.
-- * 'templateName' - The name of the fleet provisioning template version to delete.
mkDeleteProvisioningTemplateVersion ::
  -- | 'versionId'
  Lude.Int ->
  -- | 'templateName'
  Lude.Text ->
  DeleteProvisioningTemplateVersion
mkDeleteProvisioningTemplateVersion pVersionId_ pTemplateName_ =
  DeleteProvisioningTemplateVersion'
    { versionId = pVersionId_,
      templateName = pTemplateName_
    }

-- | The fleet provisioning template version ID to delete.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dVersionId :: Lens.Lens' DeleteProvisioningTemplateVersion Lude.Int
dVersionId = Lens.lens (versionId :: DeleteProvisioningTemplateVersion -> Lude.Int) (\s a -> s {versionId = a} :: DeleteProvisioningTemplateVersion)
{-# DEPRECATED dVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the fleet provisioning template version to delete.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTemplateName :: Lens.Lens' DeleteProvisioningTemplateVersion Lude.Text
dTemplateName = Lens.lens (templateName :: DeleteProvisioningTemplateVersion -> Lude.Text) (\s a -> s {templateName = a} :: DeleteProvisioningTemplateVersion)
{-# DEPRECATED dTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest DeleteProvisioningTemplateVersion where
  type
    Rs DeleteProvisioningTemplateVersion =
      DeleteProvisioningTemplateVersionResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteProvisioningTemplateVersionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProvisioningTemplateVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteProvisioningTemplateVersion where
  toPath DeleteProvisioningTemplateVersion' {..} =
    Lude.mconcat
      [ "/provisioning-templates/",
        Lude.toBS templateName,
        "/versions/",
        Lude.toBS versionId
      ]

instance Lude.ToQuery DeleteProvisioningTemplateVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProvisioningTemplateVersionResponse' smart constructor.
newtype DeleteProvisioningTemplateVersionResponse = DeleteProvisioningTemplateVersionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProvisioningTemplateVersionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteProvisioningTemplateVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProvisioningTemplateVersionResponse
mkDeleteProvisioningTemplateVersionResponse pResponseStatus_ =
  DeleteProvisioningTemplateVersionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptvfrsResponseStatus :: Lens.Lens' DeleteProvisioningTemplateVersionResponse Lude.Int
dptvfrsResponseStatus = Lens.lens (responseStatus :: DeleteProvisioningTemplateVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProvisioningTemplateVersionResponse)
{-# DEPRECATED dptvfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
