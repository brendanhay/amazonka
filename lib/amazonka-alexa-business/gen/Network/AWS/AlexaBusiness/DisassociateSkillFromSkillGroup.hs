{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a skill from a skill group.
module Network.AWS.AlexaBusiness.DisassociateSkillFromSkillGroup
  ( -- * Creating a request
    DisassociateSkillFromSkillGroup (..),
    mkDisassociateSkillFromSkillGroup,

    -- ** Request lenses
    dsfsgSkillGroupARN,
    dsfsgSkillId,

    -- * Destructuring the response
    DisassociateSkillFromSkillGroupResponse (..),
    mkDisassociateSkillFromSkillGroupResponse,

    -- ** Response lenses
    dsfsgrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateSkillFromSkillGroup' smart constructor.
data DisassociateSkillFromSkillGroup = DisassociateSkillFromSkillGroup'
  { skillGroupARN ::
      Lude.Maybe Lude.Text,
    skillId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateSkillFromSkillGroup' with the minimum fields required to make a request.
--
-- * 'skillGroupARN' - The unique identifier of a skill. Required.
-- * 'skillId' - The ARN of a skill group to associate to a skill.
mkDisassociateSkillFromSkillGroup ::
  -- | 'skillId'
  Lude.Text ->
  DisassociateSkillFromSkillGroup
mkDisassociateSkillFromSkillGroup pSkillId_ =
  DisassociateSkillFromSkillGroup'
    { skillGroupARN = Lude.Nothing,
      skillId = pSkillId_
    }

-- | The unique identifier of a skill. Required.
--
-- /Note:/ Consider using 'skillGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfsgSkillGroupARN :: Lens.Lens' DisassociateSkillFromSkillGroup (Lude.Maybe Lude.Text)
dsfsgSkillGroupARN = Lens.lens (skillGroupARN :: DisassociateSkillFromSkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupARN = a} :: DisassociateSkillFromSkillGroup)
{-# DEPRECATED dsfsgSkillGroupARN "Use generic-lens or generic-optics with 'skillGroupARN' instead." #-}

-- | The ARN of a skill group to associate to a skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfsgSkillId :: Lens.Lens' DisassociateSkillFromSkillGroup Lude.Text
dsfsgSkillId = Lens.lens (skillId :: DisassociateSkillFromSkillGroup -> Lude.Text) (\s a -> s {skillId = a} :: DisassociateSkillFromSkillGroup)
{-# DEPRECATED dsfsgSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Lude.AWSRequest DisassociateSkillFromSkillGroup where
  type
    Rs DisassociateSkillFromSkillGroup =
      DisassociateSkillFromSkillGroupResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateSkillFromSkillGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateSkillFromSkillGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.DisassociateSkillFromSkillGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateSkillFromSkillGroup where
  toJSON DisassociateSkillFromSkillGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SkillGroupArn" Lude..=) Lude.<$> skillGroupARN,
            Lude.Just ("SkillId" Lude..= skillId)
          ]
      )

instance Lude.ToPath DisassociateSkillFromSkillGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateSkillFromSkillGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateSkillFromSkillGroupResponse' smart constructor.
newtype DisassociateSkillFromSkillGroupResponse = DisassociateSkillFromSkillGroupResponse'
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

-- | Creates a value of 'DisassociateSkillFromSkillGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateSkillFromSkillGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateSkillFromSkillGroupResponse
mkDisassociateSkillFromSkillGroupResponse pResponseStatus_ =
  DisassociateSkillFromSkillGroupResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfsgrsResponseStatus :: Lens.Lens' DisassociateSkillFromSkillGroupResponse Lude.Int
dsfsgrsResponseStatus = Lens.lens (responseStatus :: DisassociateSkillFromSkillGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateSkillFromSkillGroupResponse)
{-# DEPRECATED dsfsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
