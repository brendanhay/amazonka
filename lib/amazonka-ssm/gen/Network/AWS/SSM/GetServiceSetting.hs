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
-- Module      : Network.AWS.SSM.GetServiceSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @ServiceSetting@ is an account-level setting for an AWS service. This setting defines how a user interacts with or uses a service or a feature of a service. For example, if an AWS service charges money to the account based on feature or service usage, then the AWS service team might create a default setting of "false". This means the user can't use this feature unless they change the setting to "true" and intentionally opt in for a paid feature.
--
--
-- Services map a @SettingId@ object to a setting value. AWS services teams define the default value for a @SettingId@ . You can't create a new @SettingId@ , but you can overwrite the default value if you have the @ssm:UpdateServiceSetting@ permission for the setting. Use the 'UpdateServiceSetting' API action to change the default setting. Or use the 'ResetServiceSetting' to change the value back to the original value defined by the AWS service team.
--
-- Query the current service setting for the account.
module Network.AWS.SSM.GetServiceSetting
  ( -- * Creating a Request
    getServiceSetting,
    GetServiceSetting,

    -- * Request Lenses
    gssSettingId,

    -- * Destructuring the Response
    getServiceSettingResponse,
    GetServiceSettingResponse,

    -- * Response Lenses
    gssrsServiceSetting,
    gssrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | The request body of the GetServiceSetting API action.
--
--
--
-- /See:/ 'getServiceSetting' smart constructor.
newtype GetServiceSetting = GetServiceSetting'
  { _gssSettingId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetServiceSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssSettingId' - The ID of the service setting to get. The setting ID can be @/ssm/parameter-store/default-parameter-tier@ , @/ssm/parameter-store/high-throughput-enabled@ , or @/ssm/managed-instance/activation-tier@ .
getServiceSetting ::
  -- | 'gssSettingId'
  Text ->
  GetServiceSetting
getServiceSetting pSettingId_ =
  GetServiceSetting' {_gssSettingId = pSettingId_}

-- | The ID of the service setting to get. The setting ID can be @/ssm/parameter-store/default-parameter-tier@ , @/ssm/parameter-store/high-throughput-enabled@ , or @/ssm/managed-instance/activation-tier@ .
gssSettingId :: Lens' GetServiceSetting Text
gssSettingId = lens _gssSettingId (\s a -> s {_gssSettingId = a})

instance AWSRequest GetServiceSetting where
  type Rs GetServiceSetting = GetServiceSettingResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          GetServiceSettingResponse'
            <$> (x .?> "ServiceSetting") <*> (pure (fromEnum s))
      )

instance Hashable GetServiceSetting

instance NFData GetServiceSetting

instance ToHeaders GetServiceSetting where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.GetServiceSetting" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetServiceSetting where
  toJSON GetServiceSetting' {..} =
    object (catMaybes [Just ("SettingId" .= _gssSettingId)])

instance ToPath GetServiceSetting where
  toPath = const "/"

instance ToQuery GetServiceSetting where
  toQuery = const mempty

-- | The query result body of the GetServiceSetting API action.
--
--
--
-- /See:/ 'getServiceSettingResponse' smart constructor.
data GetServiceSettingResponse = GetServiceSettingResponse'
  { _gssrsServiceSetting ::
      !(Maybe ServiceSetting),
    _gssrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetServiceSettingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssrsServiceSetting' - The query result of the current service setting.
--
-- * 'gssrsResponseStatus' - -- | The response status code.
getServiceSettingResponse ::
  -- | 'gssrsResponseStatus'
  Int ->
  GetServiceSettingResponse
getServiceSettingResponse pResponseStatus_ =
  GetServiceSettingResponse'
    { _gssrsServiceSetting = Nothing,
      _gssrsResponseStatus = pResponseStatus_
    }

-- | The query result of the current service setting.
gssrsServiceSetting :: Lens' GetServiceSettingResponse (Maybe ServiceSetting)
gssrsServiceSetting = lens _gssrsServiceSetting (\s a -> s {_gssrsServiceSetting = a})

-- | -- | The response status code.
gssrsResponseStatus :: Lens' GetServiceSettingResponse Int
gssrsResponseStatus = lens _gssrsResponseStatus (\s a -> s {_gssrsResponseStatus = a})

instance NFData GetServiceSettingResponse
