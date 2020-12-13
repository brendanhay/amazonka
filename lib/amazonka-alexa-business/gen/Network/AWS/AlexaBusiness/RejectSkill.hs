{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.RejectSkill
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill from the organization under a user's AWS account. If the skill is a private skill, it moves to an AcceptStatus of PENDING. Any private or public skill that is rejected can be added later by calling the ApproveSkill API.
module Network.AWS.AlexaBusiness.RejectSkill
  ( -- * Creating a request
    RejectSkill (..),
    mkRejectSkill,

    -- ** Request lenses
    rsSkillId,

    -- * Destructuring the response
    RejectSkillResponse (..),
    mkRejectSkillResponse,

    -- ** Response lenses
    rsrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRejectSkill' smart constructor.
newtype RejectSkill = RejectSkill'
  { -- | The unique identifier of the skill.
    skillId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectSkill' with the minimum fields required to make a request.
--
-- * 'skillId' - The unique identifier of the skill.
mkRejectSkill ::
  -- | 'skillId'
  Lude.Text ->
  RejectSkill
mkRejectSkill pSkillId_ = RejectSkill' {skillId = pSkillId_}

-- | The unique identifier of the skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsSkillId :: Lens.Lens' RejectSkill Lude.Text
rsSkillId = Lens.lens (skillId :: RejectSkill -> Lude.Text) (\s a -> s {skillId = a} :: RejectSkill)
{-# DEPRECATED rsSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Lude.AWSRequest RejectSkill where
  type Rs RejectSkill = RejectSkillResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RejectSkillResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RejectSkill where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.RejectSkill" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RejectSkill where
  toJSON RejectSkill' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SkillId" Lude..= skillId)])

instance Lude.ToPath RejectSkill where
  toPath = Lude.const "/"

instance Lude.ToQuery RejectSkill where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRejectSkillResponse' smart constructor.
newtype RejectSkillResponse = RejectSkillResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RejectSkillResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRejectSkillResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RejectSkillResponse
mkRejectSkillResponse pResponseStatus_ =
  RejectSkillResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsrsResponseStatus :: Lens.Lens' RejectSkillResponse Lude.Int
rsrsResponseStatus = Lens.lens (responseStatus :: RejectSkillResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RejectSkillResponse)
{-# DEPRECATED rsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
