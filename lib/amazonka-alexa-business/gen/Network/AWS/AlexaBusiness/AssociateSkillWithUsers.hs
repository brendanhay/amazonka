{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.AssociateSkillWithUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a private skill available for enrolled users to enable on their devices.
module Network.AWS.AlexaBusiness.AssociateSkillWithUsers
  ( -- * Creating a request
    AssociateSkillWithUsers (..),
    mkAssociateSkillWithUsers,

    -- ** Request lenses
    aswuSkillId,

    -- * Destructuring the response
    AssociateSkillWithUsersResponse (..),
    mkAssociateSkillWithUsersResponse,

    -- ** Response lenses
    aswursResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateSkillWithUsers' smart constructor.
newtype AssociateSkillWithUsers = AssociateSkillWithUsers'
  { -- | The private skill ID you want to make available to enrolled users.
    skillId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateSkillWithUsers' with the minimum fields required to make a request.
--
-- * 'skillId' - The private skill ID you want to make available to enrolled users.
mkAssociateSkillWithUsers ::
  -- | 'skillId'
  Lude.Text ->
  AssociateSkillWithUsers
mkAssociateSkillWithUsers pSkillId_ =
  AssociateSkillWithUsers' {skillId = pSkillId_}

-- | The private skill ID you want to make available to enrolled users.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswuSkillId :: Lens.Lens' AssociateSkillWithUsers Lude.Text
aswuSkillId = Lens.lens (skillId :: AssociateSkillWithUsers -> Lude.Text) (\s a -> s {skillId = a} :: AssociateSkillWithUsers)
{-# DEPRECATED aswuSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Lude.AWSRequest AssociateSkillWithUsers where
  type Rs AssociateSkillWithUsers = AssociateSkillWithUsersResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AssociateSkillWithUsersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssociateSkillWithUsers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.AssociateSkillWithUsers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssociateSkillWithUsers where
  toJSON AssociateSkillWithUsers' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SkillId" Lude..= skillId)])

instance Lude.ToPath AssociateSkillWithUsers where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateSkillWithUsers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssociateSkillWithUsersResponse' smart constructor.
newtype AssociateSkillWithUsersResponse = AssociateSkillWithUsersResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateSkillWithUsersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAssociateSkillWithUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssociateSkillWithUsersResponse
mkAssociateSkillWithUsersResponse pResponseStatus_ =
  AssociateSkillWithUsersResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aswursResponseStatus :: Lens.Lens' AssociateSkillWithUsersResponse Lude.Int
aswursResponseStatus = Lens.lens (responseStatus :: AssociateSkillWithUsersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateSkillWithUsersResponse)
{-# DEPRECATED aswursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
