{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified approval rule template. Deleting a template does not remove approval rules on pull requests already created with the template.
module Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
  ( -- * Creating a request
    DeleteApprovalRuleTemplate (..),
    mkDeleteApprovalRuleTemplate,

    -- ** Request lenses
    dartApprovalRuleTemplateName,

    -- * Destructuring the response
    DeleteApprovalRuleTemplateResponse (..),
    mkDeleteApprovalRuleTemplateResponse,

    -- ** Response lenses
    dartrsApprovalRuleTemplateId,
    dartrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteApprovalRuleTemplate' smart constructor.
newtype DeleteApprovalRuleTemplate = DeleteApprovalRuleTemplate'
  { -- | The name of the approval rule template to delete.
    approvalRuleTemplateName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApprovalRuleTemplate' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplateName' - The name of the approval rule template to delete.
mkDeleteApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Lude.Text ->
  DeleteApprovalRuleTemplate
mkDeleteApprovalRuleTemplate pApprovalRuleTemplateName_ =
  DeleteApprovalRuleTemplate'
    { approvalRuleTemplateName =
        pApprovalRuleTemplateName_
    }

-- | The name of the approval rule template to delete.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dartApprovalRuleTemplateName :: Lens.Lens' DeleteApprovalRuleTemplate Lude.Text
dartApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: DeleteApprovalRuleTemplate -> Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: DeleteApprovalRuleTemplate)
{-# DEPRECATED dartApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

instance Lude.AWSRequest DeleteApprovalRuleTemplate where
  type
    Rs DeleteApprovalRuleTemplate =
      DeleteApprovalRuleTemplateResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteApprovalRuleTemplateResponse'
            Lude.<$> (x Lude..:> "approvalRuleTemplateId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteApprovalRuleTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.DeleteApprovalRuleTemplate" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApprovalRuleTemplate where
  toJSON DeleteApprovalRuleTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("approvalRuleTemplateName" Lude..= approvalRuleTemplateName)
          ]
      )

instance Lude.ToPath DeleteApprovalRuleTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApprovalRuleTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteApprovalRuleTemplateResponse' smart constructor.
data DeleteApprovalRuleTemplateResponse = DeleteApprovalRuleTemplateResponse'
  { -- | The system-generated ID of the deleted approval rule template. If the template has been previously deleted, the only response is a 200 OK.
    approvalRuleTemplateId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApprovalRuleTemplateResponse' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplateId' - The system-generated ID of the deleted approval rule template. If the template has been previously deleted, the only response is a 200 OK.
-- * 'responseStatus' - The response status code.
mkDeleteApprovalRuleTemplateResponse ::
  -- | 'approvalRuleTemplateId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DeleteApprovalRuleTemplateResponse
mkDeleteApprovalRuleTemplateResponse
  pApprovalRuleTemplateId_
  pResponseStatus_ =
    DeleteApprovalRuleTemplateResponse'
      { approvalRuleTemplateId =
          pApprovalRuleTemplateId_,
        responseStatus = pResponseStatus_
      }

-- | The system-generated ID of the deleted approval rule template. If the template has been previously deleted, the only response is a 200 OK.
--
-- /Note:/ Consider using 'approvalRuleTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dartrsApprovalRuleTemplateId :: Lens.Lens' DeleteApprovalRuleTemplateResponse Lude.Text
dartrsApprovalRuleTemplateId = Lens.lens (approvalRuleTemplateId :: DeleteApprovalRuleTemplateResponse -> Lude.Text) (\s a -> s {approvalRuleTemplateId = a} :: DeleteApprovalRuleTemplateResponse)
{-# DEPRECATED dartrsApprovalRuleTemplateId "Use generic-lens or generic-optics with 'approvalRuleTemplateId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dartrsResponseStatus :: Lens.Lens' DeleteApprovalRuleTemplateResponse Lude.Int
dartrsResponseStatus = Lens.lens (responseStatus :: DeleteApprovalRuleTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteApprovalRuleTemplateResponse)
{-# DEPRECATED dartrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
