{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a skill with a skill group.
module Network.AWS.AlexaBusiness.AssociateSkillWithSkillGroup
  ( -- * Creating a request
    AssociateSkillWithSkillGroup (..),
    mkAssociateSkillWithSkillGroup,

    -- ** Request lenses
    aswsgSkillGroupARN,
    aswsgSkillId,

    -- * Destructuring the response
    AssociateSkillWithSkillGroupResponse (..),
    mkAssociateSkillWithSkillGroupResponse,

    -- ** Response lenses
    aswsgrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateSkillWithSkillGroup' smart constructor.
data AssociateSkillWithSkillGroup = AssociateSkillWithSkillGroup'
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

-- | Creates a value of 'AssociateSkillWithSkillGroup' with the minimum fields required to make a request.
--
-- * 'skillGroupARN' - The ARN of the skill group to associate the skill to. Required.
-- * 'skillId' - The unique identifier of the skill.
mkAssociateSkillWithSkillGroup ::
  -- | 'skillId'
  Lude.Text ->
  AssociateSkillWithSkillGroup
mkAssociateSkillWithSkillGroup pSkillId_ =
  AssociateSkillWithSkillGroup'
    { skillGroupARN = Lude.Nothing,
      skillId = pSkillId_
    }

-- | The ARN of the skill group to associate the skill to. Required.
--
-- /Note:/ Consider using 'skillGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswsgSkillGroupARN :: Lens.Lens' AssociateSkillWithSkillGroup (Lude.Maybe Lude.Text)
aswsgSkillGroupARN = Lens.lens (skillGroupARN :: AssociateSkillWithSkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupARN = a} :: AssociateSkillWithSkillGroup)
{-# DEPRECATED aswsgSkillGroupARN "Use generic-lens or generic-optics with 'skillGroupARN' instead." #-}

-- | The unique identifier of the skill.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswsgSkillId :: Lens.Lens' AssociateSkillWithSkillGroup Lude.Text
aswsgSkillId = Lens.lens (skillId :: AssociateSkillWithSkillGroup -> Lude.Text) (\s a -> s {skillId = a} :: AssociateSkillWithSkillGroup)
{-# DEPRECATED aswsgSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Lude.AWSRequest AssociateSkillWithSkillGroup where
  type
    Rs AssociateSkillWithSkillGroup =
      AssociateSkillWithSkillGroupResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateSkillWithSkillGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateSkillWithSkillGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AlexaForBusiness.AssociateSkillWithSkillGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateSkillWithSkillGroup where
  toJSON AssociateSkillWithSkillGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SkillGroupArn" Lude..=) Lude.<$> skillGroupARN,
            Lude.Just ("SkillId" Lude..= skillId)
          ]
      )

instance Lude.ToPath AssociateSkillWithSkillGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateSkillWithSkillGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateSkillWithSkillGroupResponse' smart constructor.
newtype AssociateSkillWithSkillGroupResponse = AssociateSkillWithSkillGroupResponse'
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

-- | Creates a value of 'AssociateSkillWithSkillGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateSkillWithSkillGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateSkillWithSkillGroupResponse
mkAssociateSkillWithSkillGroupResponse pResponseStatus_ =
  AssociateSkillWithSkillGroupResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswsgrsResponseStatus :: Lens.Lens' AssociateSkillWithSkillGroupResponse Lude.Int
aswsgrsResponseStatus = Lens.lens (responseStatus :: AssociateSkillWithSkillGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateSkillWithSkillGroupResponse)
{-# DEPRECATED aswsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
