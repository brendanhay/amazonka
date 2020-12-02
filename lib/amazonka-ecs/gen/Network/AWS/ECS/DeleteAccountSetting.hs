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
-- Module      : Network.AWS.ECS.DeleteAccountSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an account setting for a specified IAM user, IAM role, or the root user for an account.
module Network.AWS.ECS.DeleteAccountSetting
  ( -- * Creating a Request
    deleteAccountSetting,
    DeleteAccountSetting,

    -- * Request Lenses
    dasPrincipalARN,
    dasName,

    -- * Destructuring the Response
    deleteAccountSettingResponse,
    DeleteAccountSettingResponse,

    -- * Response Lenses
    dasrsSetting,
    dasrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAccountSetting' smart constructor.
data DeleteAccountSetting = DeleteAccountSetting'
  { _dasPrincipalARN ::
      !(Maybe Text),
    _dasName :: !SettingName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAccountSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasPrincipalARN' - The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it disables the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
--
-- * 'dasName' - The resource name for which to disable the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected.
deleteAccountSetting ::
  -- | 'dasName'
  SettingName ->
  DeleteAccountSetting
deleteAccountSetting pName_ =
  DeleteAccountSetting'
    { _dasPrincipalARN = Nothing,
      _dasName = pName_
    }

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it disables the account setting for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings. If this field is omitted, the setting is changed only for the authenticated user.
dasPrincipalARN :: Lens' DeleteAccountSetting (Maybe Text)
dasPrincipalARN = lens _dasPrincipalARN (\s a -> s {_dasPrincipalARN = a})

-- | The resource name for which to disable the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected.
dasName :: Lens' DeleteAccountSetting SettingName
dasName = lens _dasName (\s a -> s {_dasName = a})

instance AWSRequest DeleteAccountSetting where
  type Rs DeleteAccountSetting = DeleteAccountSettingResponse
  request = postJSON ecs
  response =
    receiveJSON
      ( \s h x ->
          DeleteAccountSettingResponse'
            <$> (x .?> "setting") <*> (pure (fromEnum s))
      )

instance Hashable DeleteAccountSetting

instance NFData DeleteAccountSetting

instance ToHeaders DeleteAccountSetting where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerServiceV20141113.DeleteAccountSetting" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAccountSetting where
  toJSON DeleteAccountSetting' {..} =
    object
      ( catMaybes
          [ ("principalArn" .=) <$> _dasPrincipalARN,
            Just ("name" .= _dasName)
          ]
      )

instance ToPath DeleteAccountSetting where
  toPath = const "/"

instance ToQuery DeleteAccountSetting where
  toQuery = const mempty

-- | /See:/ 'deleteAccountSettingResponse' smart constructor.
data DeleteAccountSettingResponse = DeleteAccountSettingResponse'
  { _dasrsSetting ::
      !(Maybe Setting),
    _dasrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAccountSettingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasrsSetting' - The account setting for the specified principal ARN.
--
-- * 'dasrsResponseStatus' - -- | The response status code.
deleteAccountSettingResponse ::
  -- | 'dasrsResponseStatus'
  Int ->
  DeleteAccountSettingResponse
deleteAccountSettingResponse pResponseStatus_ =
  DeleteAccountSettingResponse'
    { _dasrsSetting = Nothing,
      _dasrsResponseStatus = pResponseStatus_
    }

-- | The account setting for the specified principal ARN.
dasrsSetting :: Lens' DeleteAccountSettingResponse (Maybe Setting)
dasrsSetting = lens _dasrsSetting (\s a -> s {_dasrsSetting = a})

-- | -- | The response status code.
dasrsResponseStatus :: Lens' DeleteAccountSettingResponse Int
dasrsResponseStatus = lens _dasrsResponseStatus (\s a -> s {_dasrsResponseStatus = a})

instance NFData DeleteAccountSettingResponse
