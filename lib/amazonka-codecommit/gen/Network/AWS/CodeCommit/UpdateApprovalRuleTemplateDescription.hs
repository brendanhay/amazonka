{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description for a specified approval rule template.
module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription
  ( -- * Creating a request
    UpdateApprovalRuleTemplateDescription (..),
    mkUpdateApprovalRuleTemplateDescription,

    -- ** Request lenses
    uartdApprovalRuleTemplateDescription,
    uartdApprovalRuleTemplateName,

    -- * Destructuring the response
    UpdateApprovalRuleTemplateDescriptionResponse (..),
    mkUpdateApprovalRuleTemplateDescriptionResponse,

    -- ** Response lenses
    uartdrsApprovalRuleTemplate,
    uartdrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateApprovalRuleTemplateDescription' smart constructor.
data UpdateApprovalRuleTemplateDescription = UpdateApprovalRuleTemplateDescription'
  { -- | The updated description of the approval rule template.
    approvalRuleTemplateDescription :: Lude.Text,
    -- | The name of the template for which you want to update the description.
    approvalRuleTemplateName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApprovalRuleTemplateDescription' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplateDescription' - The updated description of the approval rule template.
-- * 'approvalRuleTemplateName' - The name of the template for which you want to update the description.
mkUpdateApprovalRuleTemplateDescription ::
  -- | 'approvalRuleTemplateDescription'
  Lude.Text ->
  -- | 'approvalRuleTemplateName'
  Lude.Text ->
  UpdateApprovalRuleTemplateDescription
mkUpdateApprovalRuleTemplateDescription
  pApprovalRuleTemplateDescription_
  pApprovalRuleTemplateName_ =
    UpdateApprovalRuleTemplateDescription'
      { approvalRuleTemplateDescription =
          pApprovalRuleTemplateDescription_,
        approvalRuleTemplateName = pApprovalRuleTemplateName_
      }

-- | The updated description of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplateDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartdApprovalRuleTemplateDescription :: Lens.Lens' UpdateApprovalRuleTemplateDescription Lude.Text
uartdApprovalRuleTemplateDescription = Lens.lens (approvalRuleTemplateDescription :: UpdateApprovalRuleTemplateDescription -> Lude.Text) (\s a -> s {approvalRuleTemplateDescription = a} :: UpdateApprovalRuleTemplateDescription)
{-# DEPRECATED uartdApprovalRuleTemplateDescription "Use generic-lens or generic-optics with 'approvalRuleTemplateDescription' instead." #-}

-- | The name of the template for which you want to update the description.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartdApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateDescription Lude.Text
uartdApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: UpdateApprovalRuleTemplateDescription -> Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: UpdateApprovalRuleTemplateDescription)
{-# DEPRECATED uartdApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

instance Lude.AWSRequest UpdateApprovalRuleTemplateDescription where
  type
    Rs UpdateApprovalRuleTemplateDescription =
      UpdateApprovalRuleTemplateDescriptionResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateDescriptionResponse'
            Lude.<$> (x Lude..:> "approvalRuleTemplate")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateApprovalRuleTemplateDescription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.UpdateApprovalRuleTemplateDescription" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateApprovalRuleTemplateDescription where
  toJSON UpdateApprovalRuleTemplateDescription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "approvalRuleTemplateDescription"
                  Lude..= approvalRuleTemplateDescription
              ),
            Lude.Just
              ("approvalRuleTemplateName" Lude..= approvalRuleTemplateName)
          ]
      )

instance Lude.ToPath UpdateApprovalRuleTemplateDescription where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApprovalRuleTemplateDescription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateApprovalRuleTemplateDescriptionResponse' smart constructor.
data UpdateApprovalRuleTemplateDescriptionResponse = UpdateApprovalRuleTemplateDescriptionResponse'
  { -- | The structure and content of the updated approval rule template.
    approvalRuleTemplate :: ApprovalRuleTemplate,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApprovalRuleTemplateDescriptionResponse' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplate' - The structure and content of the updated approval rule template.
-- * 'responseStatus' - The response status code.
mkUpdateApprovalRuleTemplateDescriptionResponse ::
  -- | 'approvalRuleTemplate'
  ApprovalRuleTemplate ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateApprovalRuleTemplateDescriptionResponse
mkUpdateApprovalRuleTemplateDescriptionResponse
  pApprovalRuleTemplate_
  pResponseStatus_ =
    UpdateApprovalRuleTemplateDescriptionResponse'
      { approvalRuleTemplate =
          pApprovalRuleTemplate_,
        responseStatus = pResponseStatus_
      }

-- | The structure and content of the updated approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartdrsApprovalRuleTemplate :: Lens.Lens' UpdateApprovalRuleTemplateDescriptionResponse ApprovalRuleTemplate
uartdrsApprovalRuleTemplate = Lens.lens (approvalRuleTemplate :: UpdateApprovalRuleTemplateDescriptionResponse -> ApprovalRuleTemplate) (\s a -> s {approvalRuleTemplate = a} :: UpdateApprovalRuleTemplateDescriptionResponse)
{-# DEPRECATED uartdrsApprovalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartdrsResponseStatus :: Lens.Lens' UpdateApprovalRuleTemplateDescriptionResponse Lude.Int
uartdrsResponseStatus = Lens.lens (responseStatus :: UpdateApprovalRuleTemplateDescriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateApprovalRuleTemplateDescriptionResponse)
{-# DEPRECATED uartdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
