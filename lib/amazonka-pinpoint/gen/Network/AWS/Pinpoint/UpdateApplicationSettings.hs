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
-- Module      : Network.AWS.Pinpoint.UpdateApplicationSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to update the settings for an app.
module Network.AWS.Pinpoint.UpdateApplicationSettings
    (
    -- * Creating a Request
      updateApplicationSettings
    , UpdateApplicationSettings
    -- * Request Lenses
    , uasApplicationId
    , uasWriteApplicationSettingsRequest

    -- * Destructuring the Response
    , updateApplicationSettingsResponse
    , UpdateApplicationSettingsResponse
    -- * Response Lenses
    , uasrsResponseStatus
    , uasrsApplicationSettingsResource
    ) where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types
import Network.AWS.Pinpoint.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateApplicationSettings' smart constructor.
data UpdateApplicationSettings = UpdateApplicationSettings'
  { _uasApplicationId                   :: !Text
  , _uasWriteApplicationSettingsRequest :: !WriteApplicationSettingsRequest
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplicationSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasApplicationId' - Undocumented member.
--
-- * 'uasWriteApplicationSettingsRequest' - Undocumented member.
updateApplicationSettings
    :: Text -- ^ 'uasApplicationId'
    -> WriteApplicationSettingsRequest -- ^ 'uasWriteApplicationSettingsRequest'
    -> UpdateApplicationSettings
updateApplicationSettings pApplicationId_ pWriteApplicationSettingsRequest_ =
  UpdateApplicationSettings'
    { _uasApplicationId = pApplicationId_
    , _uasWriteApplicationSettingsRequest = pWriteApplicationSettingsRequest_
    }


-- | Undocumented member.
uasApplicationId :: Lens' UpdateApplicationSettings Text
uasApplicationId = lens _uasApplicationId (\ s a -> s{_uasApplicationId = a})

-- | Undocumented member.
uasWriteApplicationSettingsRequest :: Lens' UpdateApplicationSettings WriteApplicationSettingsRequest
uasWriteApplicationSettingsRequest = lens _uasWriteApplicationSettingsRequest (\ s a -> s{_uasWriteApplicationSettingsRequest = a})

instance AWSRequest UpdateApplicationSettings where
        type Rs UpdateApplicationSettings =
             UpdateApplicationSettingsResponse
        request = putJSON pinpoint
        response
          = receiveJSON
              (\ s h x ->
                 UpdateApplicationSettingsResponse' <$>
                   (pure (fromEnum s)) <*> (eitherParseJSON x))

instance Hashable UpdateApplicationSettings where

instance NFData UpdateApplicationSettings where

instance ToHeaders UpdateApplicationSettings where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateApplicationSettings where
        toJSON UpdateApplicationSettings'{..}
          = object
              (catMaybes
                 [Just
                    ("WriteApplicationSettingsRequest" .=
                       _uasWriteApplicationSettingsRequest)])

instance ToPath UpdateApplicationSettings where
        toPath UpdateApplicationSettings'{..}
          = mconcat
              ["/v1/apps/", toBS _uasApplicationId, "/settings"]

instance ToQuery UpdateApplicationSettings where
        toQuery = const mempty

-- | /See:/ 'updateApplicationSettingsResponse' smart constructor.
data UpdateApplicationSettingsResponse = UpdateApplicationSettingsResponse'
  { _uasrsResponseStatus              :: !Int
  , _uasrsApplicationSettingsResource :: !ApplicationSettingsResource
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplicationSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uasrsResponseStatus' - -- | The response status code.
--
-- * 'uasrsApplicationSettingsResource' - Undocumented member.
updateApplicationSettingsResponse
    :: Int -- ^ 'uasrsResponseStatus'
    -> ApplicationSettingsResource -- ^ 'uasrsApplicationSettingsResource'
    -> UpdateApplicationSettingsResponse
updateApplicationSettingsResponse pResponseStatus_ pApplicationSettingsResource_ =
  UpdateApplicationSettingsResponse'
    { _uasrsResponseStatus = pResponseStatus_
    , _uasrsApplicationSettingsResource = pApplicationSettingsResource_
    }


-- | -- | The response status code.
uasrsResponseStatus :: Lens' UpdateApplicationSettingsResponse Int
uasrsResponseStatus = lens _uasrsResponseStatus (\ s a -> s{_uasrsResponseStatus = a})

-- | Undocumented member.
uasrsApplicationSettingsResource :: Lens' UpdateApplicationSettingsResponse ApplicationSettingsResource
uasrsApplicationSettingsResource = lens _uasrsApplicationSettingsResource (\ s a -> s{_uasrsApplicationSettingsResource = a})

instance NFData UpdateApplicationSettingsResponse
         where
