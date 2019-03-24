{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.PutAccountSetting
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the ARN and resource ID format of a resource type for a specified IAM user, IAM role, or the root user for an account. If the account setting for the root user is changed, it sets the default setting for all of the IAM users and roles for which no individual account setting has been set. The opt-in and opt-out account setting can be set for each Amazon ECS resource separately. The ARN and resource ID format of a resource will be defined by the opt-in status of the IAM user or role that created the resource. Enabling this setting is required to use new Amazon ECS features such as resource tagging. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-resource-ids.html Amazon Resource Names (ARNs) and IDs> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
module Network.AWS.ECS.PutAccountSetting
    (
    -- * Creating a Request
      putAccountSetting
    , PutAccountSetting
    -- * Request Lenses
    , pasPrincipalARN
    , pasName
    , pasValue

    -- * Destructuring the Response
    , putAccountSettingResponse
    , PutAccountSettingResponse
    -- * Response Lenses
    , pasrsSetting
    , pasrsResponseStatus
    ) where

import Network.AWS.ECS.Types
import Network.AWS.ECS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putAccountSetting' smart constructor.
data PutAccountSetting = PutAccountSetting'
  { _pasPrincipalARN :: !(Maybe Text)
  , _pasName         :: !SettingName
  , _pasValue        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAccountSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pasPrincipalARN' - The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it modifies the ARN and resource ID format for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings for themselves. If this field is omitted, the settings are changed only for the authenticated user.
--
-- * 'pasName' - The resource name for which to enable the new format. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected.
--
-- * 'pasValue' - The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
putAccountSetting
    :: SettingName -- ^ 'pasName'
    -> Text -- ^ 'pasValue'
    -> PutAccountSetting
putAccountSetting pName_ pValue_ =
  PutAccountSetting'
    {_pasPrincipalARN = Nothing, _pasName = pName_, _pasValue = pValue_}


-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If you specify the root user, it modifies the ARN and resource ID format for all IAM users, IAM roles, and the root user of the account unless an IAM user or role explicitly overrides these settings for themselves. If this field is omitted, the settings are changed only for the authenticated user.
pasPrincipalARN :: Lens' PutAccountSetting (Maybe Text)
pasPrincipalARN = lens _pasPrincipalARN (\ s a -> s{_pasPrincipalARN = a})

-- | The resource name for which to enable the new format. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected.
pasName :: Lens' PutAccountSetting SettingName
pasName = lens _pasName (\ s a -> s{_pasName = a})

-- | The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
pasValue :: Lens' PutAccountSetting Text
pasValue = lens _pasValue (\ s a -> s{_pasValue = a})

instance AWSRequest PutAccountSetting where
        type Rs PutAccountSetting = PutAccountSettingResponse
        request = postJSON ecs
        response
          = receiveJSON
              (\ s h x ->
                 PutAccountSettingResponse' <$>
                   (x .?> "setting") <*> (pure (fromEnum s)))

instance Hashable PutAccountSetting where

instance NFData PutAccountSetting where

instance ToHeaders PutAccountSetting where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonEC2ContainerServiceV20141113.PutAccountSetting"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutAccountSetting where
        toJSON PutAccountSetting'{..}
          = object
              (catMaybes
                 [("principalArn" .=) <$> _pasPrincipalARN,
                  Just ("name" .= _pasName),
                  Just ("value" .= _pasValue)])

instance ToPath PutAccountSetting where
        toPath = const "/"

instance ToQuery PutAccountSetting where
        toQuery = const mempty

-- | /See:/ 'putAccountSettingResponse' smart constructor.
data PutAccountSettingResponse = PutAccountSettingResponse'
  { _pasrsSetting        :: !(Maybe Setting)
  , _pasrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAccountSettingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pasrsSetting' - The current account setting for a resource.
--
-- * 'pasrsResponseStatus' - -- | The response status code.
putAccountSettingResponse
    :: Int -- ^ 'pasrsResponseStatus'
    -> PutAccountSettingResponse
putAccountSettingResponse pResponseStatus_ =
  PutAccountSettingResponse'
    {_pasrsSetting = Nothing, _pasrsResponseStatus = pResponseStatus_}


-- | The current account setting for a resource.
pasrsSetting :: Lens' PutAccountSettingResponse (Maybe Setting)
pasrsSetting = lens _pasrsSetting (\ s a -> s{_pasrsSetting = a})

-- | -- | The response status code.
pasrsResponseStatus :: Lens' PutAccountSettingResponse Int
pasrsResponseStatus = lens _pasrsResponseStatus (\ s a -> s{_pasrsResponseStatus = a})

instance NFData PutAccountSettingResponse where
