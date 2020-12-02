{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    updateUserPhoneConfig,
    UpdateUserPhoneConfig,

    -- * Request Lenses
    uupcPhoneConfig,
    uupcUserId,
    uupcInstanceId,

    -- * Destructuring the Response
    updateUserPhoneConfigResponse,
    UpdateUserPhoneConfigResponse,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserPhoneConfig' smart constructor.
data UpdateUserPhoneConfig = UpdateUserPhoneConfig'
  { _uupcPhoneConfig ::
      !UserPhoneConfig,
    _uupcUserId :: !Text,
    _uupcInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserPhoneConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupcPhoneConfig' - Information about phone configuration settings for the user.
--
-- * 'uupcUserId' - The identifier of the user account.
--
-- * 'uupcInstanceId' - The identifier of the Amazon Connect instance.
updateUserPhoneConfig ::
  -- | 'uupcPhoneConfig'
  UserPhoneConfig ->
  -- | 'uupcUserId'
  Text ->
  -- | 'uupcInstanceId'
  Text ->
  UpdateUserPhoneConfig
updateUserPhoneConfig pPhoneConfig_ pUserId_ pInstanceId_ =
  UpdateUserPhoneConfig'
    { _uupcPhoneConfig = pPhoneConfig_,
      _uupcUserId = pUserId_,
      _uupcInstanceId = pInstanceId_
    }

-- | Information about phone configuration settings for the user.
uupcPhoneConfig :: Lens' UpdateUserPhoneConfig UserPhoneConfig
uupcPhoneConfig = lens _uupcPhoneConfig (\s a -> s {_uupcPhoneConfig = a})

-- | The identifier of the user account.
uupcUserId :: Lens' UpdateUserPhoneConfig Text
uupcUserId = lens _uupcUserId (\s a -> s {_uupcUserId = a})

-- | The identifier of the Amazon Connect instance.
uupcInstanceId :: Lens' UpdateUserPhoneConfig Text
uupcInstanceId = lens _uupcInstanceId (\s a -> s {_uupcInstanceId = a})

instance AWSRequest UpdateUserPhoneConfig where
  type Rs UpdateUserPhoneConfig = UpdateUserPhoneConfigResponse
  request = postJSON connect
  response = receiveNull UpdateUserPhoneConfigResponse'

instance Hashable UpdateUserPhoneConfig

instance NFData UpdateUserPhoneConfig

instance ToHeaders UpdateUserPhoneConfig where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateUserPhoneConfig where
  toJSON UpdateUserPhoneConfig' {..} =
    object (catMaybes [Just ("PhoneConfig" .= _uupcPhoneConfig)])

instance ToPath UpdateUserPhoneConfig where
  toPath UpdateUserPhoneConfig' {..} =
    mconcat
      [ "/users/",
        toBS _uupcInstanceId,
        "/",
        toBS _uupcUserId,
        "/phone-config"
      ]

instance ToQuery UpdateUserPhoneConfig where
  toQuery = const mempty

-- | /See:/ 'updateUserPhoneConfigResponse' smart constructor.
data UpdateUserPhoneConfigResponse = UpdateUserPhoneConfigResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateUserPhoneConfigResponse' with the minimum fields required to make a request.
updateUserPhoneConfigResponse ::
  UpdateUserPhoneConfigResponse
updateUserPhoneConfigResponse = UpdateUserPhoneConfigResponse'

instance NFData UpdateUserPhoneConfigResponse
