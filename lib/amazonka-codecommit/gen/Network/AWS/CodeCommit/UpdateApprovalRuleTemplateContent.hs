{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateApprovalRuleTemplateContent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the content of an approval rule template. You can change the number of required approvals, the membership of the approval rule, and whether an approval pool is defined.
module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateContent
  ( -- * Creating a request
    UpdateApprovalRuleTemplateContent (..),
    mkUpdateApprovalRuleTemplateContent,

    -- ** Request lenses
    uartcExistingRuleContentSha256,
    uartcApprovalRuleTemplateName,
    uartcNewRuleContent,

    -- * Destructuring the response
    UpdateApprovalRuleTemplateContentResponse (..),
    mkUpdateApprovalRuleTemplateContentResponse,

    -- ** Response lenses
    uartcrsResponseStatus,
    uartcrsApprovalRuleTemplate,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateApprovalRuleTemplateContent' smart constructor.
data UpdateApprovalRuleTemplateContent = UpdateApprovalRuleTemplateContent'
  { existingRuleContentSha256 ::
      Lude.Maybe Lude.Text,
    approvalRuleTemplateName ::
      Lude.Text,
    newRuleContent ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApprovalRuleTemplateContent' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplateName' - The name of the approval rule template where you want to update the content of the rule.
-- * 'existingRuleContentSha256' - The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
-- * 'newRuleContent' - The content that replaces the existing content of the rule. Content statements must be complete. You cannot provide only the changes.
mkUpdateApprovalRuleTemplateContent ::
  -- | 'approvalRuleTemplateName'
  Lude.Text ->
  -- | 'newRuleContent'
  Lude.Text ->
  UpdateApprovalRuleTemplateContent
mkUpdateApprovalRuleTemplateContent
  pApprovalRuleTemplateName_
  pNewRuleContent_ =
    UpdateApprovalRuleTemplateContent'
      { existingRuleContentSha256 =
          Lude.Nothing,
        approvalRuleTemplateName = pApprovalRuleTemplateName_,
        newRuleContent = pNewRuleContent_
      }

-- | The SHA-256 hash signature for the content of the approval rule. You can retrieve this information by using 'GetPullRequest' .
--
-- /Note:/ Consider using 'existingRuleContentSha256' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartcExistingRuleContentSha256 :: Lens.Lens' UpdateApprovalRuleTemplateContent (Lude.Maybe Lude.Text)
uartcExistingRuleContentSha256 = Lens.lens (existingRuleContentSha256 :: UpdateApprovalRuleTemplateContent -> Lude.Maybe Lude.Text) (\s a -> s {existingRuleContentSha256 = a} :: UpdateApprovalRuleTemplateContent)
{-# DEPRECATED uartcExistingRuleContentSha256 "Use generic-lens or generic-optics with 'existingRuleContentSha256' instead." #-}

-- | The name of the approval rule template where you want to update the content of the rule.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartcApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateContent Lude.Text
uartcApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: UpdateApprovalRuleTemplateContent -> Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: UpdateApprovalRuleTemplateContent)
{-# DEPRECATED uartcApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

-- | The content that replaces the existing content of the rule. Content statements must be complete. You cannot provide only the changes.
--
-- /Note:/ Consider using 'newRuleContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartcNewRuleContent :: Lens.Lens' UpdateApprovalRuleTemplateContent Lude.Text
uartcNewRuleContent = Lens.lens (newRuleContent :: UpdateApprovalRuleTemplateContent -> Lude.Text) (\s a -> s {newRuleContent = a} :: UpdateApprovalRuleTemplateContent)
{-# DEPRECATED uartcNewRuleContent "Use generic-lens or generic-optics with 'newRuleContent' instead." #-}

instance Lude.AWSRequest UpdateApprovalRuleTemplateContent where
  type
    Rs UpdateApprovalRuleTemplateContent =
      UpdateApprovalRuleTemplateContentResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateContentResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "approvalRuleTemplate")
      )

instance Lude.ToHeaders UpdateApprovalRuleTemplateContent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.UpdateApprovalRuleTemplateContent" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateApprovalRuleTemplateContent where
  toJSON UpdateApprovalRuleTemplateContent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("existingRuleContentSha256" Lude..=)
              Lude.<$> existingRuleContentSha256,
            Lude.Just
              ("approvalRuleTemplateName" Lude..= approvalRuleTemplateName),
            Lude.Just ("newRuleContent" Lude..= newRuleContent)
          ]
      )

instance Lude.ToPath UpdateApprovalRuleTemplateContent where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApprovalRuleTemplateContent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateApprovalRuleTemplateContentResponse' smart constructor.
data UpdateApprovalRuleTemplateContentResponse = UpdateApprovalRuleTemplateContentResponse'
  { responseStatus ::
      Lude.Int,
    approvalRuleTemplate ::
      ApprovalRuleTemplate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApprovalRuleTemplateContentResponse' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplate' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkUpdateApprovalRuleTemplateContentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'approvalRuleTemplate'
  ApprovalRuleTemplate ->
  UpdateApprovalRuleTemplateContentResponse
mkUpdateApprovalRuleTemplateContentResponse
  pResponseStatus_
  pApprovalRuleTemplate_ =
    UpdateApprovalRuleTemplateContentResponse'
      { responseStatus =
          pResponseStatus_,
        approvalRuleTemplate = pApprovalRuleTemplate_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartcrsResponseStatus :: Lens.Lens' UpdateApprovalRuleTemplateContentResponse Lude.Int
uartcrsResponseStatus = Lens.lens (responseStatus :: UpdateApprovalRuleTemplateContentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateApprovalRuleTemplateContentResponse)
{-# DEPRECATED uartcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartcrsApprovalRuleTemplate :: Lens.Lens' UpdateApprovalRuleTemplateContentResponse ApprovalRuleTemplate
uartcrsApprovalRuleTemplate = Lens.lens (approvalRuleTemplate :: UpdateApprovalRuleTemplateContentResponse -> ApprovalRuleTemplate) (\s a -> s {approvalRuleTemplate = a} :: UpdateApprovalRuleTemplateContentResponse)
{-# DEPRECATED uartcrsApprovalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead." #-}
