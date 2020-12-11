{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    delTemplateName,
    delVersionId,

    -- * Destructuring the response
    DeleteProvisioningTemplateVersionResponse (..),
    mkDeleteProvisioningTemplateVersionResponse,

    -- ** Response lenses
    dptvprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProvisioningTemplateVersion' smart constructor.
data DeleteProvisioningTemplateVersion = DeleteProvisioningTemplateVersion'
  { templateName ::
      Lude.Text,
    versionId :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProvisioningTemplateVersion' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the fleet provisioning template version to delete.
-- * 'versionId' - The fleet provisioning template version ID to delete.
mkDeleteProvisioningTemplateVersion ::
  -- | 'templateName'
  Lude.Text ->
  -- | 'versionId'
  Lude.Int ->
  DeleteProvisioningTemplateVersion
mkDeleteProvisioningTemplateVersion pTemplateName_ pVersionId_ =
  DeleteProvisioningTemplateVersion'
    { templateName = pTemplateName_,
      versionId = pVersionId_
    }

-- | The name of the fleet provisioning template version to delete.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delTemplateName :: Lens.Lens' DeleteProvisioningTemplateVersion Lude.Text
delTemplateName = Lens.lens (templateName :: DeleteProvisioningTemplateVersion -> Lude.Text) (\s a -> s {templateName = a} :: DeleteProvisioningTemplateVersion)
{-# DEPRECATED delTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

-- | The fleet provisioning template version ID to delete.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delVersionId :: Lens.Lens' DeleteProvisioningTemplateVersion Lude.Int
delVersionId = Lens.lens (versionId :: DeleteProvisioningTemplateVersion -> Lude.Int) (\s a -> s {versionId = a} :: DeleteProvisioningTemplateVersion)
{-# DEPRECATED delVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

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
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
dptvprsResponseStatus :: Lens.Lens' DeleteProvisioningTemplateVersionResponse Lude.Int
dptvprsResponseStatus = Lens.lens (responseStatus :: DeleteProvisioningTemplateVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProvisioningTemplateVersionResponse)
{-# DEPRECATED dptvprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
