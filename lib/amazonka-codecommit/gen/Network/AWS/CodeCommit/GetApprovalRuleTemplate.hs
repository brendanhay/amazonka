{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetApprovalRuleTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified approval rule template.
module Network.AWS.CodeCommit.GetApprovalRuleTemplate
  ( -- * Creating a request
    GetApprovalRuleTemplate (..),
    mkGetApprovalRuleTemplate,

    -- ** Request lenses
    gartApprovalRuleTemplateName,

    -- * Destructuring the response
    GetApprovalRuleTemplateResponse (..),
    mkGetApprovalRuleTemplateResponse,

    -- ** Response lenses
    gartrsResponseStatus,
    gartrsApprovalRuleTemplate,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetApprovalRuleTemplate' smart constructor.
newtype GetApprovalRuleTemplate = GetApprovalRuleTemplate'
  { approvalRuleTemplateName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApprovalRuleTemplate' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplateName' - The name of the approval rule template for which you want to get information.
mkGetApprovalRuleTemplate ::
  -- | 'approvalRuleTemplateName'
  Lude.Text ->
  GetApprovalRuleTemplate
mkGetApprovalRuleTemplate pApprovalRuleTemplateName_ =
  GetApprovalRuleTemplate'
    { approvalRuleTemplateName =
        pApprovalRuleTemplateName_
    }

-- | The name of the approval rule template for which you want to get information.
--
-- /Note:/ Consider using 'approvalRuleTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gartApprovalRuleTemplateName :: Lens.Lens' GetApprovalRuleTemplate Lude.Text
gartApprovalRuleTemplateName = Lens.lens (approvalRuleTemplateName :: GetApprovalRuleTemplate -> Lude.Text) (\s a -> s {approvalRuleTemplateName = a} :: GetApprovalRuleTemplate)
{-# DEPRECATED gartApprovalRuleTemplateName "Use generic-lens or generic-optics with 'approvalRuleTemplateName' instead." #-}

instance Lude.AWSRequest GetApprovalRuleTemplate where
  type Rs GetApprovalRuleTemplate = GetApprovalRuleTemplateResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetApprovalRuleTemplateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "approvalRuleTemplate")
      )

instance Lude.ToHeaders GetApprovalRuleTemplate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetApprovalRuleTemplate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetApprovalRuleTemplate where
  toJSON GetApprovalRuleTemplate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("approvalRuleTemplateName" Lude..= approvalRuleTemplateName)
          ]
      )

instance Lude.ToPath GetApprovalRuleTemplate where
  toPath = Lude.const "/"

instance Lude.ToQuery GetApprovalRuleTemplate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetApprovalRuleTemplateResponse' smart constructor.
data GetApprovalRuleTemplateResponse = GetApprovalRuleTemplateResponse'
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

-- | Creates a value of 'GetApprovalRuleTemplateResponse' with the minimum fields required to make a request.
--
-- * 'approvalRuleTemplate' - The content and structure of the approval rule template.
-- * 'responseStatus' - The response status code.
mkGetApprovalRuleTemplateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'approvalRuleTemplate'
  ApprovalRuleTemplate ->
  GetApprovalRuleTemplateResponse
mkGetApprovalRuleTemplateResponse
  pResponseStatus_
  pApprovalRuleTemplate_ =
    GetApprovalRuleTemplateResponse'
      { responseStatus =
          pResponseStatus_,
        approvalRuleTemplate = pApprovalRuleTemplate_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gartrsResponseStatus :: Lens.Lens' GetApprovalRuleTemplateResponse Lude.Int
gartrsResponseStatus = Lens.lens (responseStatus :: GetApprovalRuleTemplateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetApprovalRuleTemplateResponse)
{-# DEPRECATED gartrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The content and structure of the approval rule template.
--
-- /Note:/ Consider using 'approvalRuleTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gartrsApprovalRuleTemplate :: Lens.Lens' GetApprovalRuleTemplateResponse ApprovalRuleTemplate
gartrsApprovalRuleTemplate = Lens.lens (approvalRuleTemplate :: GetApprovalRuleTemplateResponse -> ApprovalRuleTemplate) (\s a -> s {approvalRuleTemplate = a} :: GetApprovalRuleTemplateResponse)
{-# DEPRECATED gartrsApprovalRuleTemplate "Use generic-lens or generic-optics with 'approvalRuleTemplate' instead." #-}
