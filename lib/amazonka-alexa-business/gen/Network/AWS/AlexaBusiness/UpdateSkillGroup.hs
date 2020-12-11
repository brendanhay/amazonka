{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates skill group details by skill group ARN.
module Network.AWS.AlexaBusiness.UpdateSkillGroup
  ( -- * Creating a request
    UpdateSkillGroup (..),
    mkUpdateSkillGroup,

    -- ** Request lenses
    usgSkillGroupARN,
    usgDescription,
    usgSkillGroupName,

    -- * Destructuring the response
    UpdateSkillGroupResponse (..),
    mkUpdateSkillGroupResponse,

    -- ** Response lenses
    usgrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateSkillGroup' smart constructor.
data UpdateSkillGroup = UpdateSkillGroup'
  { skillGroupARN ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    skillGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSkillGroup' with the minimum fields required to make a request.
--
-- * 'description' - The updated description for the skill group.
-- * 'skillGroupARN' - The ARN of the skill group to update.
-- * 'skillGroupName' - The updated name for the skill group.
mkUpdateSkillGroup ::
  UpdateSkillGroup
mkUpdateSkillGroup =
  UpdateSkillGroup'
    { skillGroupARN = Lude.Nothing,
      description = Lude.Nothing,
      skillGroupName = Lude.Nothing
    }

-- | The ARN of the skill group to update.
--
-- /Note:/ Consider using 'skillGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgSkillGroupARN :: Lens.Lens' UpdateSkillGroup (Lude.Maybe Lude.Text)
usgSkillGroupARN = Lens.lens (skillGroupARN :: UpdateSkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupARN = a} :: UpdateSkillGroup)
{-# DEPRECATED usgSkillGroupARN "Use generic-lens or generic-optics with 'skillGroupARN' instead." #-}

-- | The updated description for the skill group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgDescription :: Lens.Lens' UpdateSkillGroup (Lude.Maybe Lude.Text)
usgDescription = Lens.lens (description :: UpdateSkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateSkillGroup)
{-# DEPRECATED usgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The updated name for the skill group.
--
-- /Note:/ Consider using 'skillGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgSkillGroupName :: Lens.Lens' UpdateSkillGroup (Lude.Maybe Lude.Text)
usgSkillGroupName = Lens.lens (skillGroupName :: UpdateSkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupName = a} :: UpdateSkillGroup)
{-# DEPRECATED usgSkillGroupName "Use generic-lens or generic-optics with 'skillGroupName' instead." #-}

instance Lude.AWSRequest UpdateSkillGroup where
  type Rs UpdateSkillGroup = UpdateSkillGroupResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateSkillGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSkillGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.UpdateSkillGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSkillGroup where
  toJSON UpdateSkillGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SkillGroupArn" Lude..=) Lude.<$> skillGroupARN,
            ("Description" Lude..=) Lude.<$> description,
            ("SkillGroupName" Lude..=) Lude.<$> skillGroupName
          ]
      )

instance Lude.ToPath UpdateSkillGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSkillGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSkillGroupResponse' smart constructor.
newtype UpdateSkillGroupResponse = UpdateSkillGroupResponse'
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

-- | Creates a value of 'UpdateSkillGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateSkillGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSkillGroupResponse
mkUpdateSkillGroupResponse pResponseStatus_ =
  UpdateSkillGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usgrsResponseStatus :: Lens.Lens' UpdateSkillGroupResponse Lude.Int
usgrsResponseStatus = Lens.lens (responseStatus :: UpdateSkillGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSkillGroupResponse)
{-# DEPRECATED usgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
