{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectoryService.EnableRadius
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Enables multi-factor authentication (MFA) with Remote Authentication
-- Dial In User Service (RADIUS) for an AD Connector directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_EnableRadius.html>
module Network.AWS.DirectoryService.EnableRadius
    (
    -- * Request
      EnableRadius
    -- ** Request constructor
    , enableRadius
    -- ** Request lenses
    , erDirectoryId
    , erRadiusSettings

    -- * Response
    , EnableRadiusResponse
    -- ** Response constructor
    , enableRadiusResponse
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableRadius' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erDirectoryId'
--
-- * 'erRadiusSettings'
data EnableRadius = EnableRadius'{_erDirectoryId :: Text, _erRadiusSettings :: RadiusSettings} deriving (Eq, Read, Show)

-- | 'EnableRadius' smart constructor.
enableRadius :: Text -> RadiusSettings -> EnableRadius
enableRadius pDirectoryId pRadiusSettings = EnableRadius'{_erDirectoryId = pDirectoryId, _erRadiusSettings = pRadiusSettings};

-- | The identifier of the directory to enable MFA for.
erDirectoryId :: Lens' EnableRadius Text
erDirectoryId = lens _erDirectoryId (\ s a -> s{_erDirectoryId = a});

-- | A RadiusSettings object that contains information about the RADIUS
-- server.
erRadiusSettings :: Lens' EnableRadius RadiusSettings
erRadiusSettings = lens _erRadiusSettings (\ s a -> s{_erRadiusSettings = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest EnableRadius where
        type Sv EnableRadius = DirectoryService
        type Rs EnableRadius = EnableRadiusResponse
        request = postJSON
        response = receiveNull EnableRadiusResponse'

instance ToHeaders EnableRadius where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.EnableRadius" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableRadius where
        toJSON EnableRadius'{..}
          = object
              ["DirectoryId" .= _erDirectoryId,
               "RadiusSettings" .= _erRadiusSettings]

instance ToPath EnableRadius where
        toPath = const "/"

instance ToQuery EnableRadius where
        toQuery = const mempty

-- | /See:/ 'enableRadiusResponse' smart constructor.
data EnableRadiusResponse = EnableRadiusResponse' deriving (Eq, Read, Show)

-- | 'EnableRadiusResponse' smart constructor.
enableRadiusResponse :: EnableRadiusResponse
enableRadiusResponse = EnableRadiusResponse';
