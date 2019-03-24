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
-- Module      : Network.AWS.SSM.UpdateServiceSetting
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @ServiceSetting@ is an account-level setting for an AWS service. This setting defines how a user interacts with or uses a service or a feature of a service. For example, if an AWS service charges money to the account based on feature or service usage, then the AWS service team might create a default setting of "false". This means the user can't use this feature unless they change the setting to "true" and intentionally opt in for a paid feature.
--
--
-- Services map a @SettingId@ object to a setting value. AWS services teams define the default value for a @SettingId@ . You can't create a new @SettingId@ , but you can overwrite the default value if you have the @ssm:UpdateServiceSetting@ permission for the setting. Use the 'GetServiceSetting' API action to view the current value. Or, use the 'ResetServiceSetting' to change the value back to the original value defined by the AWS service team.
--
-- Update the service setting for the account.
--
module Network.AWS.SSM.UpdateServiceSetting
    (
    -- * Creating a Request
      updateServiceSetting
    , UpdateServiceSetting
    -- * Request Lenses
    , ussSettingId
    , ussSettingValue

    -- * Destructuring the Response
    , updateServiceSettingResponse
    , UpdateServiceSettingResponse
    -- * Response Lenses
    , ussrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | The request body of the UpdateServiceSetting API action.
--
--
--
-- /See:/ 'updateServiceSetting' smart constructor.
data UpdateServiceSetting = UpdateServiceSetting'
  { _ussSettingId    :: !Text
  , _ussSettingValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateServiceSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussSettingId' - The ID of the service setting to update.
--
-- * 'ussSettingValue' - The new value to specify for the service setting.
updateServiceSetting
    :: Text -- ^ 'ussSettingId'
    -> Text -- ^ 'ussSettingValue'
    -> UpdateServiceSetting
updateServiceSetting pSettingId_ pSettingValue_ =
  UpdateServiceSetting'
    {_ussSettingId = pSettingId_, _ussSettingValue = pSettingValue_}


-- | The ID of the service setting to update.
ussSettingId :: Lens' UpdateServiceSetting Text
ussSettingId = lens _ussSettingId (\ s a -> s{_ussSettingId = a})

-- | The new value to specify for the service setting.
ussSettingValue :: Lens' UpdateServiceSetting Text
ussSettingValue = lens _ussSettingValue (\ s a -> s{_ussSettingValue = a})

instance AWSRequest UpdateServiceSetting where
        type Rs UpdateServiceSetting =
             UpdateServiceSettingResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateServiceSettingResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateServiceSetting where

instance NFData UpdateServiceSetting where

instance ToHeaders UpdateServiceSetting where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.UpdateServiceSetting" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateServiceSetting where
        toJSON UpdateServiceSetting'{..}
          = object
              (catMaybes
                 [Just ("SettingId" .= _ussSettingId),
                  Just ("SettingValue" .= _ussSettingValue)])

instance ToPath UpdateServiceSetting where
        toPath = const "/"

instance ToQuery UpdateServiceSetting where
        toQuery = const mempty

-- | The result body of the UpdateServiceSetting API action.
--
--
--
-- /See:/ 'updateServiceSettingResponse' smart constructor.
newtype UpdateServiceSettingResponse = UpdateServiceSettingResponse'
  { _ussrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateServiceSettingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussrsResponseStatus' - -- | The response status code.
updateServiceSettingResponse
    :: Int -- ^ 'ussrsResponseStatus'
    -> UpdateServiceSettingResponse
updateServiceSettingResponse pResponseStatus_ =
  UpdateServiceSettingResponse' {_ussrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ussrsResponseStatus :: Lens' UpdateServiceSettingResponse Int
ussrsResponseStatus = lens _ussrsResponseStatus (\ s a -> s{_ussrsResponseStatus = a})

instance NFData UpdateServiceSettingResponse where
