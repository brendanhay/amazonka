{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ApproveSkill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with the organization under the customer's AWS account. If a skill is private, the user implicitly accepts access to this skill during enablement.
module Network.AWS.AlexaBusiness.ApproveSkill
  ( -- * Creating a request
    ApproveSkill (..),
    mkApproveSkill,

    -- ** Request lenses
    asSkillId,

    -- * Destructuring the response
    ApproveSkillResponse (..),
    mkApproveSkillResponse,

    -- ** Response lenses
    asrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkApproveSkill' smart constructor.
newtype ApproveSkill = ApproveSkill' {skillId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApproveSkill' with the minimum fields required to make a request.
--
-- * 'skillId' - The unique identifier of the skill.
mkApproveSkill ::
  -- | 'skillId'
  Lude.Text ->
  ApproveSkill
mkApproveSkill pSkillId_ = ApproveSkill' {skillId = pSkillId_}

-- | The unique identifier of the skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSkillId :: Lens.Lens' ApproveSkill Lude.Text
asSkillId = Lens.lens (skillId :: ApproveSkill -> Lude.Text) (\s a -> s {skillId = a} :: ApproveSkill)
{-# DEPRECATED asSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Lude.AWSRequest ApproveSkill where
  type Rs ApproveSkill = ApproveSkillResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ApproveSkillResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ApproveSkill where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.ApproveSkill" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ApproveSkill where
  toJSON ApproveSkill' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SkillId" Lude..= skillId)])

instance Lude.ToPath ApproveSkill where
  toPath = Lude.const "/"

instance Lude.ToQuery ApproveSkill where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkApproveSkillResponse' smart constructor.
newtype ApproveSkillResponse = ApproveSkillResponse'
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

-- | Creates a value of 'ApproveSkillResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkApproveSkillResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ApproveSkillResponse
mkApproveSkillResponse pResponseStatus_ =
  ApproveSkillResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asrsResponseStatus :: Lens.Lens' ApproveSkillResponse Lude.Int
asrsResponseStatus = Lens.lens (responseStatus :: ApproveSkillResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ApproveSkillResponse)
{-# DEPRECATED asrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
