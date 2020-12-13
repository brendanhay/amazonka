{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserPhoneConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the phone configuration settings for the specified user.
module Network.AWS.Connect.UpdateUserPhoneConfig
  ( -- * Creating a request
    UpdateUserPhoneConfig (..),
    mkUpdateUserPhoneConfig,

    -- ** Request lenses
    uupcInstanceId,
    uupcUserId,
    uupcPhoneConfig,

    -- * Destructuring the response
    UpdateUserPhoneConfigResponse (..),
    mkUpdateUserPhoneConfigResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUserPhoneConfig' smart constructor.
data UpdateUserPhoneConfig = UpdateUserPhoneConfig'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Lude.Text,
    -- | The identifier of the user account.
    userId :: Lude.Text,
    -- | Information about phone configuration settings for the user.
    phoneConfig :: UserPhoneConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserPhoneConfig' with the minimum fields required to make a request.
--
-- * 'instanceId' - The identifier of the Amazon Connect instance.
-- * 'userId' - The identifier of the user account.
-- * 'phoneConfig' - Information about phone configuration settings for the user.
mkUpdateUserPhoneConfig ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'userId'
  Lude.Text ->
  -- | 'phoneConfig'
  UserPhoneConfig ->
  UpdateUserPhoneConfig
mkUpdateUserPhoneConfig pInstanceId_ pUserId_ pPhoneConfig_ =
  UpdateUserPhoneConfig'
    { instanceId = pInstanceId_,
      userId = pUserId_,
      phoneConfig = pPhoneConfig_
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcInstanceId :: Lens.Lens' UpdateUserPhoneConfig Lude.Text
uupcInstanceId = Lens.lens (instanceId :: UpdateUserPhoneConfig -> Lude.Text) (\s a -> s {instanceId = a} :: UpdateUserPhoneConfig)
{-# DEPRECATED uupcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The identifier of the user account.
--
-- /Note:/ Consider using 'userId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcUserId :: Lens.Lens' UpdateUserPhoneConfig Lude.Text
uupcUserId = Lens.lens (userId :: UpdateUserPhoneConfig -> Lude.Text) (\s a -> s {userId = a} :: UpdateUserPhoneConfig)
{-# DEPRECATED uupcUserId "Use generic-lens or generic-optics with 'userId' instead." #-}

-- | Information about phone configuration settings for the user.
--
-- /Note:/ Consider using 'phoneConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcPhoneConfig :: Lens.Lens' UpdateUserPhoneConfig UserPhoneConfig
uupcPhoneConfig = Lens.lens (phoneConfig :: UpdateUserPhoneConfig -> UserPhoneConfig) (\s a -> s {phoneConfig = a} :: UpdateUserPhoneConfig)
{-# DEPRECATED uupcPhoneConfig "Use generic-lens or generic-optics with 'phoneConfig' instead." #-}

instance Lude.AWSRequest UpdateUserPhoneConfig where
  type Rs UpdateUserPhoneConfig = UpdateUserPhoneConfigResponse
  request = Req.postJSON connectService
  response = Res.receiveNull UpdateUserPhoneConfigResponse'

instance Lude.ToHeaders UpdateUserPhoneConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserPhoneConfig where
  toJSON UpdateUserPhoneConfig' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("PhoneConfig" Lude..= phoneConfig)])

instance Lude.ToPath UpdateUserPhoneConfig where
  toPath UpdateUserPhoneConfig' {..} =
    Lude.mconcat
      [ "/users/",
        Lude.toBS instanceId,
        "/",
        Lude.toBS userId,
        "/phone-config"
      ]

instance Lude.ToQuery UpdateUserPhoneConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserPhoneConfigResponse' smart constructor.
data UpdateUserPhoneConfigResponse = UpdateUserPhoneConfigResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserPhoneConfigResponse' with the minimum fields required to make a request.
mkUpdateUserPhoneConfigResponse ::
  UpdateUserPhoneConfigResponse
mkUpdateUserPhoneConfigResponse = UpdateUserPhoneConfigResponse'
