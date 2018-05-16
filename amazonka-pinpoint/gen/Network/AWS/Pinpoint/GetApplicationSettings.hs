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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    , gassrsResponseStatus
    , gassrsApplicationSettingsResource
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getApplicationSettings' smart constructor.
newtype GetApplicationSettings = GetApplicationSettings'
  { _gasApplicationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetApplicationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasApplicationId' - Undocumented member.
getApplicationSettings
    :: Text -- ^ 'gasApplicationId'
    -> GetApplicationSettings
getApplicationSettings pApplicationId_ =
  GetApplicationSettings' {_gasApplicationId = pApplicationId_}


-- | Undocumented member.
gasApplicationId :: Lens' GetApplicationSettings Text
gasApplicationId = lens _gasApplicationId (\ s a -> s{_gasApplicationId = a})

instance AWSRequest GetApplicationSettings where
        type Rs GetApplicationSettings =
             GetApplicationSettingsResponse
        request = get pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 GetApplicationSettingsResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable GetApplicationSettings where

instance NFData GetApplicationSettings where

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
  { _gassrsResponseStatus              :: !Int
  , _gassrsApplicationSettingsResource :: !ApplicationSettingsResource
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetApplicationSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gassrsResponseStatus' - -- | The response status code.
--
-- * 'gassrsApplicationSettingsResource' - Undocumented member.
getApplicationSettingsResponse
    :: Int -- ^ 'gassrsResponseStatus'
    -> ApplicationSettingsResource -- ^ 'gassrsApplicationSettingsResource'
    -> GetApplicationSettingsResponse
getApplicationSettingsResponse pResponseStatus_ pApplicationSettingsResource_ =
  GetApplicationSettingsResponse'
    { _gassrsResponseStatus = pResponseStatus_
    , _gassrsApplicationSettingsResource = pApplicationSettingsResource_
    }


-- | -- | The response status code.
gassrsResponseStatus :: Lens' GetApplicationSettingsResponse Int
gassrsResponseStatus = lens _gassrsResponseStatus (\ s a -> s{_gassrsResponseStatus = a})

-- | Undocumented member.
gassrsApplicationSettingsResource :: Lens' GetApplicationSettingsResponse ApplicationSettingsResource
gassrsApplicationSettingsResource = lens _gassrsApplicationSettingsResource (\ s a -> s{_gassrsApplicationSettingsResource = a})

instance NFData GetApplicationSettingsResponse where
