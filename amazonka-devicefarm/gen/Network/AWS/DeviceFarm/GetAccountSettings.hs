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
-- Module      : Network.AWS.DeviceFarm.GetAccountSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of unmetered iOS and/or unmetered Android devices that have been purchased by the account.
--
--
module Network.AWS.DeviceFarm.GetAccountSettings
    (
    -- * Creating a Request
      getAccountSettings
    , GetAccountSettings

    -- * Destructuring the Response
    , getAccountSettingsResponse
    , GetAccountSettingsResponse
    -- * Response Lenses
    , gasrsAccountSettings
    , gasrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request sent to retrieve the account settings.
--
--
--
-- /See:/ 'getAccountSettings' smart constructor.
data GetAccountSettings =
  GetAccountSettings'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountSettings' with the minimum fields required to make a request.
--
getAccountSettings
    :: GetAccountSettings
getAccountSettings = GetAccountSettings'


instance AWSRequest GetAccountSettings where
        type Rs GetAccountSettings =
             GetAccountSettingsResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 GetAccountSettingsResponse' <$>
                   (x .?> "accountSettings") <*> (pure (fromEnum s)))

instance Hashable GetAccountSettings where

instance NFData GetAccountSettings where

instance ToHeaders GetAccountSettings where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetAccountSettings" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAccountSettings where
        toJSON = const (Object mempty)

instance ToPath GetAccountSettings where
        toPath = const "/"

instance ToQuery GetAccountSettings where
        toQuery = const mempty

-- | Represents the account settings return values from the @GetAccountSettings@ request.
--
--
--
-- /See:/ 'getAccountSettingsResponse' smart constructor.
data GetAccountSettingsResponse = GetAccountSettingsResponse'
  { _gasrsAccountSettings :: !(Maybe AccountSettings)
  , _gasrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasrsAccountSettings' - The account settings.
--
-- * 'gasrsResponseStatus' - -- | The response status code.
getAccountSettingsResponse
    :: Int -- ^ 'gasrsResponseStatus'
    -> GetAccountSettingsResponse
getAccountSettingsResponse pResponseStatus_ =
  GetAccountSettingsResponse'
    {_gasrsAccountSettings = Nothing, _gasrsResponseStatus = pResponseStatus_}


-- | The account settings.
gasrsAccountSettings :: Lens' GetAccountSettingsResponse (Maybe AccountSettings)
gasrsAccountSettings = lens _gasrsAccountSettings (\ s a -> s{_gasrsAccountSettings = a})

-- | -- | The response status code.
gasrsResponseStatus :: Lens' GetAccountSettingsResponse Int
gasrsResponseStatus = lens _gasrsResponseStatus (\ s a -> s{_gasrsResponseStatus = a})

instance NFData GetAccountSettingsResponse where
