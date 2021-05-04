{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateUserPhoneConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the phone configuration settings for the specified user.
module Network.AWS.Connect.UpdateUserPhoneConfig
  ( -- * Creating a Request
    UpdateUserPhoneConfig (..),
    newUpdateUserPhoneConfig,

    -- * Request Lenses
    updateUserPhoneConfig_phoneConfig,
    updateUserPhoneConfig_userId,
    updateUserPhoneConfig_instanceId,

    -- * Destructuring the Response
    UpdateUserPhoneConfigResponse (..),
    newUpdateUserPhoneConfigResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUserPhoneConfig' smart constructor.
data UpdateUserPhoneConfig = UpdateUserPhoneConfig'
  { -- | Information about phone configuration settings for the user.
    phoneConfig :: UserPhoneConfig,
    -- | The identifier of the user account.
    userId :: Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserPhoneConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneConfig', 'updateUserPhoneConfig_phoneConfig' - Information about phone configuration settings for the user.
--
-- 'userId', 'updateUserPhoneConfig_userId' - The identifier of the user account.
--
-- 'instanceId', 'updateUserPhoneConfig_instanceId' - The identifier of the Amazon Connect instance.
newUpdateUserPhoneConfig ::
  -- | 'phoneConfig'
  UserPhoneConfig ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'instanceId'
  Prelude.Text ->
  UpdateUserPhoneConfig
newUpdateUserPhoneConfig
  pPhoneConfig_
  pUserId_
  pInstanceId_ =
    UpdateUserPhoneConfig'
      { phoneConfig = pPhoneConfig_,
        userId = pUserId_,
        instanceId = pInstanceId_
      }

-- | Information about phone configuration settings for the user.
updateUserPhoneConfig_phoneConfig :: Lens.Lens' UpdateUserPhoneConfig UserPhoneConfig
updateUserPhoneConfig_phoneConfig = Lens.lens (\UpdateUserPhoneConfig' {phoneConfig} -> phoneConfig) (\s@UpdateUserPhoneConfig' {} a -> s {phoneConfig = a} :: UpdateUserPhoneConfig)

-- | The identifier of the user account.
updateUserPhoneConfig_userId :: Lens.Lens' UpdateUserPhoneConfig Prelude.Text
updateUserPhoneConfig_userId = Lens.lens (\UpdateUserPhoneConfig' {userId} -> userId) (\s@UpdateUserPhoneConfig' {} a -> s {userId = a} :: UpdateUserPhoneConfig)

-- | The identifier of the Amazon Connect instance.
updateUserPhoneConfig_instanceId :: Lens.Lens' UpdateUserPhoneConfig Prelude.Text
updateUserPhoneConfig_instanceId = Lens.lens (\UpdateUserPhoneConfig' {instanceId} -> instanceId) (\s@UpdateUserPhoneConfig' {} a -> s {instanceId = a} :: UpdateUserPhoneConfig)

instance Prelude.AWSRequest UpdateUserPhoneConfig where
  type
    Rs UpdateUserPhoneConfig =
      UpdateUserPhoneConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateUserPhoneConfigResponse'

instance Prelude.Hashable UpdateUserPhoneConfig

instance Prelude.NFData UpdateUserPhoneConfig

instance Prelude.ToHeaders UpdateUserPhoneConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateUserPhoneConfig where
  toJSON UpdateUserPhoneConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PhoneConfig" Prelude..= phoneConfig)
          ]
      )

instance Prelude.ToPath UpdateUserPhoneConfig where
  toPath UpdateUserPhoneConfig' {..} =
    Prelude.mconcat
      [ "/users/",
        Prelude.toBS instanceId,
        "/",
        Prelude.toBS userId,
        "/phone-config"
      ]

instance Prelude.ToQuery UpdateUserPhoneConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUserPhoneConfigResponse' smart constructor.
data UpdateUserPhoneConfigResponse = UpdateUserPhoneConfigResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserPhoneConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserPhoneConfigResponse ::
  UpdateUserPhoneConfigResponse
newUpdateUserPhoneConfigResponse =
  UpdateUserPhoneConfigResponse'

instance Prelude.NFData UpdateUserPhoneConfigResponse
