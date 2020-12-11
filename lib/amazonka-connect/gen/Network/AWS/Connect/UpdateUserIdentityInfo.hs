{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserIdentityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the identity information for the specified user.
--
-- /Important:/ Someone with the ability to invoke @UpdateUserIndentityInfo@ can change the login credentials of other users by changing their email address. This poses a security risk to your organization. They can change the email address of a user to the attacker's email address, and then reset the password through email. We strongly recommend limiting who has the ability to invoke @UpdateUserIndentityInfo@ . For more information, see <https://docs.aws.amazon.com/connect/latest/adminguide/security-profile-best-practices.html Best Practices for Security Profiles> in the /Amazon Connect Administrator Guide/ .
module Network.AWS.Connect.UpdateUserIdentityInfo
  ( -- * Creating a request
    UpdateUserIdentityInfo (..),
    mkUpdateUserIdentityInfo,

    -- ** Request lenses
    uuiiIdentityInfo,
    uuiiUserId,
    uuiiInstanceId,

    -- * Destructuring the response
    UpdateUserIdentityInfoResponse (..),
    mkUpdateUserIdentityInfoResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUserIdentityInfo' smart constructor.
data UpdateUserIdentityInfo = UpdateUserIdentityInfo'
  { identityInfo ::
      UserIdentityInfo,
    userId :: Lude.Text,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserIdentityInfo' with the minimum fields required to make a request.
--
-- * 'identityInfo' - The identity information for the user.
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'userId' - The identifier of the user account.
mkUpdateUserIdentityInfo ::
  -- | 'identityInfo'
  UserIdentityInfo ->
  -- | 'userId'
  Lude.Text ->
  -- | 'instanceId'
  Lude.Text ->
  UpdateUserIdentityInfo
mkUpdateUserIdentityInfo pIdentityInfo_ pUserId_ pInstanceId_ =
  UpdateUserIdentityInfo'
    { identityInfo = pIdentityInfo_,
      userId = pUserId_,
      instanceId = pInstanceId_
    }

-- | The identity information for the user.
--
-- /Note:/ Consider using 'identityInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuiiIdentityInfo :: Lens.Lens' UpdateUserIdentityInfo UserIdentityInfo
uuiiIdentityInfo = Lens.lens (identityInfo :: UpdateUserIdentityInfo -> UserIdentityInfo) (\s a -> s {identityInfo = a} :: UpdateUserIdentityInfo)
{-# DEPRECATED uuiiIdentityInfo "Use generic-lens or generic-optics with 'identityInfo' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuiiUserId :: Lens.Lens' UpdateUserIdentityInfo Lude.Text
uuiiUserId = Lens.lens (userId :: UpdateUserIdentityInfo -> Lude.Text) (\s a -> s {userId = a} :: UpdateUserIdentityInfo)
{-# DEPRECATED uuiiUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuiiInstanceId :: Lens.Lens' UpdateUserIdentityInfo Lude.Text
uuiiInstanceId = Lens.lens (instanceId :: UpdateUserIdentityInfo -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateUserIdentityInfo)
{-# DEPRECATED uuiiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest UpdateUserIdentityInfo where
  type Rs UpdateUserIdentityInfo = UpdateUserIdentityInfoResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateUserIdentityInfoResponse'

instance Lude.ToHeaders UpdateUserIdentityInfo where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserIdentityInfo where
  toJSON UpdateUserIdentityInfo' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("IdentityInfo" Lude..= identityInfo)])

instance Lude.ToPath UpdateUserIdentityInfo where
  toPath UpdateUserIdentityInfo' {..} =
    Lude.mconcat
      [ "/users/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS userId,
        "/identity-info"
      ]

instance Lude.ToQuery UpdateUserIdentityInfo where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserIdentityInfoResponse' smart constructor.
data UpdateUserIdentityInfoResponse = UpdateUserIdentityInfoResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserIdentityInfoResponse' with the minimum fields required to make a request.
mkUpdateUserIdentityInfoResponse ::
  UpdateUserIdentityInfoResponse
mkUpdateUserIdentityInfoResponse = UpdateUserIdentityInfoResponse'
