{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name of a specified approval rule template.
module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
  ( -- * Creating a request
    UpdateApprovalRuleTemplateName (..),
    mkUpdateApprovalRuleTemplateName,

    -- ** Request lenses
    uartnOldApprovalRuleTemplateName,
    uartnNewApprovalRuleTemplateName,

    -- * Destructuring the response
    UpdateApprovalRuleTemplateNameResponse (..),
    mkUpdateApprovalRuleTemplateNameResponse,

    -- ** Response lenses
    uartnrsResponseStatus,
    uartnrsApprovalRuleTemplate,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateApprovalRuleTemplateName' smart constructor.
data UpdateApprovalRuleTemplateName = UpdateApprovalRuleTemplateName'
  { oldApprovalRuleTemplateName ::
      Lude.Text,
    newApprovalRuleTemplateName ::
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

-- | Creates a value of 'UpdateApprovalRuleTemplateName' with the minimum fields required to make a request.
--
-- * 'newApprovalRuleTemplateName' - The new name you want to apply to the approval rule template.
-- * 'oldApprovalRuleTemplateName' - The current name of the approval rule template.
mkUpdateApprovalRuleTemplateName ::
  -- | 'oldApprovalRuleTemplateName'
  Lude.Text ->
  -- | 'newApprovalRuleTemplateName'
  Lude.Text ->
  UpdateApprovalRuleTemplateName
mkUpdateApprovalRuleTemplateName
  pOldApprovalRuleTemplateName_
  pNewApprovalRuleTemplateName_ =
    UpdateApprovalRuleTemplateName'
      { oldApprovalRuleTemplateName =
          pOldApprovalRuleTemplateName_,
        newApprovalRuleTemplateName = pNewApprovalRuleTemplateName_
      }

-- | The current name of the approval rule template.
--
-- /Note:/ Consider using 'oldApprovalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnOldApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateName Lude.Text
uartnOldApprovalRuleTemplateName = Lens.lens (oldApprovalRuleTemplateName :: UpdateApprovalRuleTemplateName -> Lude.Text) (\s a -> s {oldApprovalRuleTemplateName = a} :: UpdateApprovalRuleTemplateName)
{-# DEPRECATED uartnOldApprovalRuleTemplateName "Use generic-lens or generic-optics with 'oldApprovalRuleTemplateName' instead." #-}

-- | The new name you want to apply to the approval rule template.
--
-- /Note:/ Consider using 'newApprovalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnNewApprovalRuleTemplateName :: Lens.Lens' UpdateApprovalRuleTemplateName Lude.Text
uartnNewApprovalRuleTemplateName = Lens.lens (newApprovalRuleTemplateName :: UpdateApprovalRuleTemplateName -> Lude.Text) (\s a -> s {newApprovalRuleTemplateName = a} :: UpdateApprovalRuleTemplateName)
{-# DEPRECATED uartnNewApprovalRuleTemplateName "Use generic-lens or generic-optics with 'newApprovalRuleTemplateName' instead." #-}

instance Lude.AWSRequest UpdateApprovalRuleTemplateName where
  type
    Rs UpdateApprovalRuleTemplateName =
      UpdateApprovalRuleTemplateNameResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateApprovalRuleTemplateNameResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "approvalRuleTemplate")
      )

instance Lude.ToHeaders UpdateApprovalRuleTemplateName where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.UpdateApprovalRuleTemplateName" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateApprovalRuleTemplateName where
  toJSON UpdateApprovalRuleTemplateName' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "oldApprovalRuleTemplateName"
                  Lude..= oldApprovalRuleTemplateName
              ),
            Lude.Just
              ( "newApprovalRuleTemplateName"
                  Lude..= newApprovalRuleTemplateName
              )
          ]
      )

instance Lude.ToPath UpdateApprovalRuleTemplateName where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateApprovalRuleTemplateName where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateApprovalRuleTemplateNameResponse' smart constructor.
data UpdateApprovalRuleTemplateNameResponse = UpdateApprovalRuleTemplateNameResponse'
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

-- | Creates a value of 'UpdateApprovalRuleTemplateNameResponse' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplate' - The structure and content of the updated approval rule template.
-- * 'responseStatus' - The response status code.
mkUpdateApprovalRuleTemplateNameResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'approvalRuleTemplate'
  ApprovalRuleTemplate ->
  UpdateApprovalRuleTemplateNameResponse
mkUpdateApprovalRuleTemplateNameResponse
  pResponseStatus_
  pApprovalRuleTemplate_ =
    UpdateApprovalRuleTemplateNameResponse'
      { responseStatus =
          pResponseStatus_,
        approvalRuleTemplate = pApprovalRuleTemplate_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnrsResponseStatus :: Lens.Lens' UpdateApprovalRuleTemplateNameResponse Lude.Int
uartnrsResponseStatus = Lens.lens (responseStatus :: UpdateApprovalRuleTemplateNameResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateApprovalRuleTemplateNameResponse)
{-# DEPRECATED uartnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The structure and content of the updated approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uartnrsApprovalRuleTemplate :: Lens.Lens' UpdateApprovalRuleTemplateNameResponse ApprovalRuleTemplate
uartnrsApprovalRuleTemplate = Lens.lens (approvalRuleTemplate :: UpdateApprovalRuleTemplateNameResponse -> ApprovalRuleTemplate) (\s a -> s {approvalRuleTemplate = a} :: UpdateApprovalRuleTemplateNameResponse)
{-# DEPRECATED uartnrsApprovalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead." #-}
