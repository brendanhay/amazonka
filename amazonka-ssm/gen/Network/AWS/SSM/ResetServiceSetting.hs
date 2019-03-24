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
-- Module      : Network.AWS.SSM.ResetServiceSetting
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @ServiceSetting@ is an account-level setting for an AWS service. This setting defines how a user interacts with or uses a service or a feature of a service. For example, if an AWS service charges money to the account based on feature or service usage, then the AWS service team might create a default setting of "false". This means the user can't use this feature unless they change the setting to "true" and intentionally opt in for a paid feature.
--
--
-- Services map a @SettingId@ object to a setting value. AWS services teams define the default value for a @SettingId@ . You can't create a new @SettingId@ , but you can overwrite the default value if you have the @ssm:UpdateServiceSetting@ permission for the setting. Use the 'GetServiceSetting' API action to view the current value. Use the 'UpdateServiceSetting' API action to change the default setting.
--
-- Reset the service setting for the account to the default value as provisioned by the AWS service team.
--
module Network.AWS.SSM.ResetServiceSetting
    (
    -- * Creating a Request
      resetServiceSetting
    , ResetServiceSetting
    -- * Request Lenses
    , rssSettingId

    -- * Destructuring the Response
    , resetServiceSettingResponse
    , ResetServiceSettingResponse
    -- * Response Lenses
    , rssrsServiceSetting
    , rssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | The request body of the ResetServiceSetting API action.
--
--
--
-- /See:/ 'resetServiceSetting' smart constructor.
newtype ResetServiceSetting = ResetServiceSetting'
  { _rssSettingId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetServiceSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rssSettingId' - The ID of the service setting to reset.
resetServiceSetting
    :: Text -- ^ 'rssSettingId'
    -> ResetServiceSetting
resetServiceSetting pSettingId_ =
  ResetServiceSetting' {_rssSettingId = pSettingId_}


-- | The ID of the service setting to reset.
rssSettingId :: Lens' ResetServiceSetting Text
rssSettingId = lens _rssSettingId (\ s a -> s{_rssSettingId = a})

instance AWSRequest ResetServiceSetting where
        type Rs ResetServiceSetting =
             ResetServiceSettingResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ResetServiceSettingResponse' <$>
                   (x .?> "ServiceSetting") <*> (pure (fromEnum s)))

instance Hashable ResetServiceSetting where

instance NFData ResetServiceSetting where

instance ToHeaders ResetServiceSetting where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ResetServiceSetting" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResetServiceSetting where
        toJSON ResetServiceSetting'{..}
          = object
              (catMaybes [Just ("SettingId" .= _rssSettingId)])

instance ToPath ResetServiceSetting where
        toPath = const "/"

instance ToQuery ResetServiceSetting where
        toQuery = const mempty

-- | The result body of the ResetServiceSetting API action.
--
--
--
-- /See:/ 'resetServiceSettingResponse' smart constructor.
data ResetServiceSettingResponse = ResetServiceSettingResponse'
  { _rssrsServiceSetting :: !(Maybe ServiceSetting)
  , _rssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResetServiceSettingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rssrsServiceSetting' - The current, effective service setting after calling the ResetServiceSetting API action.
--
-- * 'rssrsResponseStatus' - -- | The response status code.
resetServiceSettingResponse
    :: Int -- ^ 'rssrsResponseStatus'
    -> ResetServiceSettingResponse
resetServiceSettingResponse pResponseStatus_ =
  ResetServiceSettingResponse'
    {_rssrsServiceSetting = Nothing, _rssrsResponseStatus = pResponseStatus_}


-- | The current, effective service setting after calling the ResetServiceSetting API action.
rssrsServiceSetting :: Lens' ResetServiceSettingResponse (Maybe ServiceSetting)
rssrsServiceSetting = lens _rssrsServiceSetting (\ s a -> s{_rssrsServiceSetting = a})

-- | -- | The response status code.
rssrsResponseStatus :: Lens' ResetServiceSettingResponse Int
rssrsResponseStatus = lens _rssrsResponseStatus (\ s a -> s{_rssrsResponseStatus = a})

instance NFData ResetServiceSettingResponse where
