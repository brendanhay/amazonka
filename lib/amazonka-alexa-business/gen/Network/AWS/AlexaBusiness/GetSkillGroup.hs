{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.GetSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets skill group details by skill group ARN.
module Network.AWS.AlexaBusiness.GetSkillGroup
  ( -- * Creating a request
    GetSkillGroup (..),
    mkGetSkillGroup,

    -- ** Request lenses
    gsgSkillGroupARN,

    -- * Destructuring the response
    GetSkillGroupResponse (..),
    mkGetSkillGroupResponse,

    -- ** Response lenses
    gsgrsSkillGroup,
    gsgrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSkillGroup' smart constructor.
newtype GetSkillGroup = GetSkillGroup'
  { -- | The ARN of the skill group for which to get details. Required.
    skillGroupARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSkillGroup' with the minimum fields required to make a request.
--
-- * 'skillGroupARN' - The ARN of the skill group for which to get details. Required.
mkGetSkillGroup ::
  GetSkillGroup
mkGetSkillGroup = GetSkillGroup' {skillGroupARN = Lude.Nothing}

-- | The ARN of the skill group for which to get details. Required.
--
-- /Note:/ Consider using 'skillGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgSkillGroupARN :: Lens.Lens' GetSkillGroup (Lude.Maybe Lude.Text)
gsgSkillGroupARN = Lens.lens (skillGroupARN :: GetSkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupARN = a} :: GetSkillGroup)
{-# DEPRECATED gsgSkillGroupARN "Use generic-lens or generic-optics with 'skillGroupARN' instead." #-}

instance Lude.AWSRequest GetSkillGroup where
  type Rs GetSkillGroup = GetSkillGroupResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSkillGroupResponse'
            Lude.<$> (x Lude..?> "SkillGroup") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSkillGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.GetSkillGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSkillGroup where
  toJSON GetSkillGroup' {..} =
    Lude.object
      (Lude.catMaybes [("SkillGroupArn" Lude..=) Lude.<$> skillGroupARN])

instance Lude.ToPath GetSkillGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSkillGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSkillGroupResponse' smart constructor.
data GetSkillGroupResponse = GetSkillGroupResponse'
  { -- | The details of the skill group requested. Required.
    skillGroup :: Lude.Maybe SkillGroup,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSkillGroupResponse' with the minimum fields required to make a request.
--
-- * 'skillGroup' - The details of the skill group requested. Required.
-- * 'responseStatus' - The response status code.
mkGetSkillGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSkillGroupResponse
mkGetSkillGroupResponse pResponseStatus_ =
  GetSkillGroupResponse'
    { skillGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The details of the skill group requested. Required.
--
-- /Note:/ Consider using 'skillGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrsSkillGroup :: Lens.Lens' GetSkillGroupResponse (Lude.Maybe SkillGroup)
gsgrsSkillGroup = Lens.lens (skillGroup :: GetSkillGroupResponse -> Lude.Maybe SkillGroup) (\s a -> s {skillGroup = a} :: GetSkillGroupResponse)
{-# DEPRECATED gsgrsSkillGroup "Use generic-lens or generic-optics with 'skillGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsgrsResponseStatus :: Lens.Lens' GetSkillGroupResponse Lude.Int
gsgrsResponseStatus = Lens.lens (responseStatus :: GetSkillGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSkillGroupResponse)
{-# DEPRECATED gsgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
