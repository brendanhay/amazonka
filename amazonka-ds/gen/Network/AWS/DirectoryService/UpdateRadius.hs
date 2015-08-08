{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.UpdateRadius
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Remote Authentication Dial In User Service (RADIUS) server
-- information for an AD Connector directory.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_UpdateRadius.html AWS API Reference> for UpdateRadius.
module Network.AWS.DirectoryService.UpdateRadius
    (
    -- * Creating a Request
      UpdateRadius
    , updateRadius
    -- * Request Lenses
    , urDirectoryId
    , urRadiusSettings

    -- * Destructuring the Response
    , UpdateRadiusResponse
    , updateRadiusResponse
    -- * Response Lenses
    , urrsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the UpdateRadius operation.
--
-- /See:/ 'updateRadius' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urDirectoryId'
--
-- * 'urRadiusSettings'
data UpdateRadius = UpdateRadius'
    { _urDirectoryId    :: !Text
    , _urRadiusSettings :: !RadiusSettings
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateRadius' smart constructor.
updateRadius :: Text -> RadiusSettings -> UpdateRadius
updateRadius pDirectoryId_ pRadiusSettings_ =
    UpdateRadius'
    { _urDirectoryId = pDirectoryId_
    , _urRadiusSettings = pRadiusSettings_
    }

-- | The identifier of the directory to update the RADIUS server information
-- for.
urDirectoryId :: Lens' UpdateRadius Text
urDirectoryId = lens _urDirectoryId (\ s a -> s{_urDirectoryId = a});

-- | A RadiusSettings object that contains information about the RADIUS
-- server.
urRadiusSettings :: Lens' UpdateRadius RadiusSettings
urRadiusSettings = lens _urRadiusSettings (\ s a -> s{_urRadiusSettings = a});

instance AWSRequest UpdateRadius where
        type Sv UpdateRadius = DirectoryService
        type Rs UpdateRadius = UpdateRadiusResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRadiusResponse' <$> (pure (fromEnum s)))

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
              ["DirectoryId" .= _urDirectoryId,
               "RadiusSettings" .= _urRadiusSettings]

instance ToPath UpdateRadius where
        toPath = const "/"

instance ToQuery UpdateRadius where
        toQuery = const mempty

-- | Contains the results of the UpdateRadius operation.
--
-- /See:/ 'updateRadiusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'urrsStatus'
newtype UpdateRadiusResponse = UpdateRadiusResponse'
    { _urrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateRadiusResponse' smart constructor.
updateRadiusResponse :: Int -> UpdateRadiusResponse
updateRadiusResponse pStatus_ =
    UpdateRadiusResponse'
    { _urrsStatus = pStatus_
    }

-- | Undocumented member.
urrsStatus :: Lens' UpdateRadiusResponse Int
urrsStatus = lens _urrsStatus (\ s a -> s{_urrsStatus = a});
