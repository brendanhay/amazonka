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
-- Module      : Network.AWS.Pinpoint.GetApplicationSettings
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to request the settings for an app.
module Network.AWS.Pinpoint.GetApplicationSettings
    (
    -- * Creating a Request
      getApplicationSettings
    , GetApplicationSettings
    -- * Request Lenses
    , gasApplicationId

    -- * Destructuring the Response
    , getApplicationSettingsResponse
    , GetApplicationSettingsResponse
    -- * Response Lenses
    , gasrsResponseStatus
    , gasrsApplicationSettingsResource
    ) where

import           Network.AWS.Lens
import           Network.AWS.Pinpoint.Types
import           Network.AWS.Pinpoint.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getApplicationSettings' smart constructor.
newtype GetApplicationSettings = GetApplicationSettings'
    { _gasApplicationId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetApplicationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasApplicationId' - Undocumented member.
getApplicationSettings
    :: Text -- ^ 'gasApplicationId'
    -> GetApplicationSettings
getApplicationSettings pApplicationId_ =
    GetApplicationSettings'
    { _gasApplicationId = pApplicationId_
    }

-- | Undocumented member.
gasApplicationId :: Lens' GetApplicationSettings Text
gasApplicationId = lens _gasApplicationId (\ s a -> s{_gasApplicationId = a});

instance AWSRequest GetApplicationSettings where
        type Rs GetApplicationSettings =
             GetApplicationSettingsResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetApplicationSettingsResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetApplicationSettings

instance NFData GetApplicationSettings

instance ToHeaders GetApplicationSettings where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetApplicationSettings where
        toPath GetApplicationSettings'{..}
          = mconcat
              ["/v1/apps/", toBS _gasApplicationId, "/settings"]

instance ToQuery GetApplicationSettings where
        toQuery = const mempty

-- | /See:/ 'getApplicationSettingsResponse' smart constructor.
data GetApplicationSettingsResponse = GetApplicationSettingsResponse'
    { _gasrsResponseStatus              :: !Int
    , _gasrsApplicationSettingsResource :: !ApplicationSettingsResource
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetApplicationSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasrsResponseStatus' - -- | The response status code.
--
-- * 'gasrsApplicationSettingsResource' - Undocumented member.
getApplicationSettingsResponse
    :: Int -- ^ 'gasrsResponseStatus'
    -> ApplicationSettingsResource -- ^ 'gasrsApplicationSettingsResource'
    -> GetApplicationSettingsResponse
getApplicationSettingsResponse pResponseStatus_ pApplicationSettingsResource_ =
    GetApplicationSettingsResponse'
    { _gasrsResponseStatus = pResponseStatus_
    , _gasrsApplicationSettingsResource = pApplicationSettingsResource_
    }

-- | -- | The response status code.
gasrsResponseStatus :: Lens' GetApplicationSettingsResponse Int
gasrsResponseStatus = lens _gasrsResponseStatus (\ s a -> s{_gasrsResponseStatus = a});

-- | Undocumented member.
gasrsApplicationSettingsResource :: Lens' GetApplicationSettingsResponse ApplicationSettingsResource
gasrsApplicationSettingsResource = lens _gasrsApplicationSettingsResource (\ s a -> s{_gasrsApplicationSettingsResource = a});

instance NFData GetApplicationSettingsResponse
