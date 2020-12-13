{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteProvisioningTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet provisioning template.
module Network.AWS.IoT.DeleteProvisioningTemplate
  ( -- * Creating a request
    DeleteProvisioningTemplate (..),
    mkDeleteProvisioningTemplate,

    -- ** Request lenses
    dptfTemplateName,

    -- * Destructuring the response
    DeleteProvisioningTemplateResponse (..),
    mkDeleteProvisioningTemplateResponse,

    -- ** Response lenses
    dptfrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteProvisioningTemplate' smart constructor.
newtype DeleteProvisioningTemplate = DeleteProvisioningTemplate'
  { -- | The name of the fleet provision template to delete.
    templateName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProvisioningTemplate' with the minimum fields required to make a request.
--
-- * 'templateName' - The name of the fleet provision template to delete.
mkDeleteProvisioningTemplate ::
  -- | 'templateName'
  Lude.Text ->
  DeleteProvisioningTemplate
mkDeleteProvisioningTemplate pTemplateName_ =
  DeleteProvisioningTemplate' {templateName = pTemplateName_}

-- | The name of the fleet provision template to delete.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptfTemplateName :: Lens.Lens' DeleteProvisioningTemplate Lude.Text
dptfTemplateName = Lens.lens (templateName :: DeleteProvisioningTemplate -> Lude.Text) (\s a -> s {templateName = a} :: DeleteProvisioningTemplate)
{-# DEPRECATED dptfTemplateName "Use generic-lens or generic-optics with 'templateName' instead." #-}

instance Lude.AWSRequest DeleteProvisioningTemplate where
  type
    Rs DeleteProvisioningTemplate =
      DeleteProvisioningTemplateResponse
  request = Req.delete ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteProvisioningTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProvisioningTemplate where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteProvisioningTemplate where
  toPath DeleteProvisioningTemplate' {..} =
    Lude.mconcat ["/provisioning-templates/", Lude.toBS templateName]

instance Lude.ToQuery DeleteProvisioningTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProvisioningTemplateResponse' smart constructor.
newtype DeleteProvisioningTemplateResponse = DeleteProvisioningTemplateResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProvisioningTemplateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteProvisioningTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProvisioningTemplateResponse
mkDeleteProvisioningTemplateResponse pResponseStatus_ =
  DeleteProvisioningTemplateResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptfrsResponseStatus :: Lens.Lens' DeleteProvisioningTemplateResponse Lude.Int
dptfrsResponseStatus = Lens.lens (responseStatus :: DeleteProvisioningTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProvisioningTemplateResponse)
{-# DEPRECATED dptfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
