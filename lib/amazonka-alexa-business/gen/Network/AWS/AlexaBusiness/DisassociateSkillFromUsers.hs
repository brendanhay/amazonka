{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Makes a private skill unavailable for enrolled users and prevents them from enabling it on their devices.
module Network.AWS.AlexaBusiness.DisassociateSkillFromUsers
  ( -- * Creating a request
    DisassociateSkillFromUsers (..),
    mkDisassociateSkillFromUsers,

    -- ** Request lenses
    dsfuSkillId,

    -- * Destructuring the response
    DisassociateSkillFromUsersResponse (..),
    mkDisassociateSkillFromUsersResponse,

    -- ** Response lenses
    dsfursResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisassociateSkillFromUsers' smart constructor.
newtype DisassociateSkillFromUsers = DisassociateSkillFromUsers'
  { -- | The private skill ID you want to make unavailable for enrolled users.
    skillId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateSkillFromUsers' with the minimum fields required to make a request.
--
-- * 'skillId' - The private skill ID you want to make unavailable for enrolled users.
mkDisassociateSkillFromUsers ::
  -- | 'skillId'
  Lude.Text ->
  DisassociateSkillFromUsers
mkDisassociateSkillFromUsers pSkillId_ =
  DisassociateSkillFromUsers' {skillId = pSkillId_}

-- | The private skill ID you want to make unavailable for enrolled users.
--
-- /Note:/ Consider using 'skillId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfuSkillId :: Lens.Lens' DisassociateSkillFromUsers Lude.Text
dsfuSkillId = Lens.lens (skillId :: DisassociateSkillFromUsers -> Lude.Text) (\s a -> s {skillId = a} :: DisassociateSkillFromUsers)
{-# DEPRECATED dsfuSkillId "Use generic-lens or generic-optics with 'skillId' instead." #-}

instance Lude.AWSRequest DisassociateSkillFromUsers where
  type
    Rs DisassociateSkillFromUsers =
      DisassociateSkillFromUsersResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateSkillFromUsersResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateSkillFromUsers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.DisassociateSkillFromUsers" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateSkillFromUsers where
  toJSON DisassociateSkillFromUsers' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SkillId" Lude..= skillId)])

instance Lude.ToPath DisassociateSkillFromUsers where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateSkillFromUsers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateSkillFromUsersResponse' smart constructor.
newtype DisassociateSkillFromUsersResponse = DisassociateSkillFromUsersResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateSkillFromUsersResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateSkillFromUsersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateSkillFromUsersResponse
mkDisassociateSkillFromUsersResponse pResponseStatus_ =
  DisassociateSkillFromUsersResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfursResponseStatus :: Lens.Lens' DisassociateSkillFromUsersResponse Lude.Int
dsfursResponseStatus = Lens.lens (responseStatus :: DisassociateSkillFromUsersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateSkillFromUsersResponse)
{-# DEPRECATED dsfursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
