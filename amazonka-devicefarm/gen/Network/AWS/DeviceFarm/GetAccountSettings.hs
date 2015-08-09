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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of unmetered iOS and\/or unmetered Android devices
-- that have been purchased by the account.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetAccountSettings.html AWS API Reference> for GetAccountSettings.
module Network.AWS.DeviceFarm.GetAccountSettings
    (
    -- * Creating a Request
      GetAccountSettings
    , getAccountSettings

    -- * Destructuring the Response
    , GetAccountSettingsResponse
    , getAccountSettingsResponse
    -- * Response Lenses
    , gasrsAccountSettings
    , gasrsStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAccountSettings' smart constructor.
data GetAccountSettings =
    GetAccountSettings' 
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetAccountSettings' smart constructor.
getAccountSettings :: GetAccountSettings
getAccountSettings = GetAccountSettings'

instance AWSRequest GetAccountSettings where
        type Sv GetAccountSettings = DeviceFarm
        type Rs GetAccountSettings =
             GetAccountSettingsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetAccountSettingsResponse' <$>
                   (x .?> "accountSettings") <*> (pure (fromEnum s)))

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

-- | /See:/ 'getAccountSettingsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gasrsAccountSettings'
--
-- * 'gasrsStatus'
data GetAccountSettingsResponse = GetAccountSettingsResponse'
    { _gasrsAccountSettings :: !(Maybe AccountSettings)
    , _gasrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetAccountSettingsResponse' smart constructor.
getAccountSettingsResponse :: Int -> GetAccountSettingsResponse
getAccountSettingsResponse pStatus_ = 
    GetAccountSettingsResponse'
    { _gasrsAccountSettings = Nothing
    , _gasrsStatus = pStatus_
    }

-- | Undocumented member.
gasrsAccountSettings :: Lens' GetAccountSettingsResponse (Maybe AccountSettings)
gasrsAccountSettings = lens _gasrsAccountSettings (\ s a -> s{_gasrsAccountSettings = a});

-- | Undocumented member.
gasrsStatus :: Lens' GetAccountSettingsResponse Int
gasrsStatus = lens _gasrsStatus (\ s a -> s{_gasrsStatus = a});
