{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.CreateSkillGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a skill group with a specified name and description.
module Network.AWS.AlexaBusiness.CreateSkillGroup
  ( -- * Creating a request
    CreateSkillGroup (..),
    mkCreateSkillGroup,

    -- ** Request lenses
    csgClientRequestToken,
    csgDescription,
    csgTags,
    csgSkillGroupName,

    -- * Destructuring the response
    CreateSkillGroupResponse (..),
    mkCreateSkillGroupResponse,

    -- ** Response lenses
    csgrsSkillGroupARN,
    csgrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateSkillGroup' smart constructor.
data CreateSkillGroup = CreateSkillGroup'
  { clientRequestToken ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    skillGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSkillGroup' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique, user-specified identifier for this request that ensures idempotency.
-- * 'description' - The description for the skill group.
-- * 'skillGroupName' - The name for the skill group.
-- * 'tags' - The tags for the skill group.
mkCreateSkillGroup ::
  -- | 'skillGroupName'
  Lude.Text ->
  CreateSkillGroup
mkCreateSkillGroup pSkillGroupName_ =
  CreateSkillGroup'
    { clientRequestToken = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      skillGroupName = pSkillGroupName_
    }

-- | A unique, user-specified identifier for this request that ensures idempotency.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgClientRequestToken :: Lens.Lens' CreateSkillGroup (Lude.Maybe Lude.Text)
csgClientRequestToken = Lens.lens (clientRequestToken :: CreateSkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: CreateSkillGroup)
{-# DEPRECATED csgClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The description for the skill group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDescription :: Lens.Lens' CreateSkillGroup (Lude.Maybe Lude.Text)
csgDescription = Lens.lens (description :: CreateSkillGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateSkillGroup)
{-# DEPRECATED csgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The tags for the skill group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgTags :: Lens.Lens' CreateSkillGroup (Lude.Maybe [Tag])
csgTags = Lens.lens (tags :: CreateSkillGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateSkillGroup)
{-# DEPRECATED csgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name for the skill group.
--
-- /Note:/ Consider using 'skillGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSkillGroupName :: Lens.Lens' CreateSkillGroup Lude.Text
csgSkillGroupName = Lens.lens (skillGroupName :: CreateSkillGroup -> Lude.Text) (\s a -> s {skillGroupName = a} :: CreateSkillGroup)
{-# DEPRECATED csgSkillGroupName "Use generic-lens or generic-optics with 'skillGroupName' instead." #-}

instance Lude.AWSRequest CreateSkillGroup where
  type Rs CreateSkillGroup = CreateSkillGroupResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateSkillGroupResponse'
            Lude.<$> (x Lude..?> "SkillGroupArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateSkillGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.CreateSkillGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateSkillGroup where
  toJSON CreateSkillGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("SkillGroupName" Lude..= skillGroupName)
          ]
      )

instance Lude.ToPath CreateSkillGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateSkillGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateSkillGroupResponse' smart constructor.
data CreateSkillGroupResponse = CreateSkillGroupResponse'
  { skillGroupARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateSkillGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'skillGroupARN' - The ARN of the newly created skill group in the response.
mkCreateSkillGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateSkillGroupResponse
mkCreateSkillGroupResponse pResponseStatus_ =
  CreateSkillGroupResponse'
    { skillGroupARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the newly created skill group in the response.
--
-- /Note:/ Consider using 'skillGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrsSkillGroupARN :: Lens.Lens' CreateSkillGroupResponse (Lude.Maybe Lude.Text)
csgrsSkillGroupARN = Lens.lens (skillGroupARN :: CreateSkillGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {skillGroupARN = a} :: CreateSkillGroupResponse)
{-# DEPRECATED csgrsSkillGroupARN "Use generic-lens or generic-optics with 'skillGroupARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgrsResponseStatus :: Lens.Lens' CreateSkillGroupResponse Lude.Int
csgrsResponseStatus = Lens.lens (responseStatus :: CreateSkillGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateSkillGroupResponse)
{-# DEPRECATED csgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
