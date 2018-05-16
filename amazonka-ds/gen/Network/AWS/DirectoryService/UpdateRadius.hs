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
-- Module      : Network.AWS.DirectoryService.UpdateRadius
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Remote Authentication Dial In User Service (RADIUS) server information for an AD Connector directory.
--
--
module Network.AWS.DirectoryService.UpdateRadius
    (
    -- * Creating a Request
      updateRadius
    , UpdateRadius
    -- * Request Lenses
    , urDirectoryId
    , urRadiusSettings

    -- * Destructuring the Response
    , updateRadiusResponse
    , UpdateRadiusResponse
    -- * Response Lenses
    , urrsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'UpdateRadius' operation.
--
--
--
-- /See:/ 'updateRadius' smart constructor.
data UpdateRadius = UpdateRadius'
  { _urDirectoryId    :: !Text
  , _urRadiusSettings :: !RadiusSettings
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRadius' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urDirectoryId' - The identifier of the directory for which to update the RADIUS server information.
--
-- * 'urRadiusSettings' - A 'RadiusSettings' object that contains information about the RADIUS server.
updateRadius
    :: Text -- ^ 'urDirectoryId'
    -> RadiusSettings -- ^ 'urRadiusSettings'
    -> UpdateRadius
updateRadius pDirectoryId_ pRadiusSettings_ =
  UpdateRadius'
    {_urDirectoryId = pDirectoryId_, _urRadiusSettings = pRadiusSettings_}


-- | The identifier of the directory for which to update the RADIUS server information.
urDirectoryId :: Lens' UpdateRadius Text
urDirectoryId = lens _urDirectoryId (\ s a -> s{_urDirectoryId = a})

-- | A 'RadiusSettings' object that contains information about the RADIUS server.
urRadiusSettings :: Lens' UpdateRadius RadiusSettings
urRadiusSettings = lens _urRadiusSettings (\ s a -> s{_urRadiusSettings = a})

instance AWSRequest UpdateRadius where
        type Rs UpdateRadius = UpdateRadiusResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateRadiusResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateRadius where

instance NFData UpdateRadius where

instance ToHeaders UpdateRadius where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.UpdateRadius" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRadius where
        toJSON UpdateRadius'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _urDirectoryId),
                  Just ("RadiusSettings" .= _urRadiusSettings)])

instance ToPath UpdateRadius where
        toPath = const "/"

instance ToQuery UpdateRadius where
        toQuery = const mempty

-- | Contains the results of the 'UpdateRadius' operation.
--
--
--
-- /See:/ 'updateRadiusResponse' smart constructor.
newtype UpdateRadiusResponse = UpdateRadiusResponse'
  { _urrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRadiusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsResponseStatus' - -- | The response status code.
updateRadiusResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UpdateRadiusResponse
updateRadiusResponse pResponseStatus_ =
  UpdateRadiusResponse' {_urrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
urrsResponseStatus :: Lens' UpdateRadiusResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UpdateRadiusResponse where
