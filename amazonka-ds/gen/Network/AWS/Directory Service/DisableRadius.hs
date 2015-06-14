{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Directory Service.DisableRadius
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

-- | Disables multi-factor authentication (MFA) with Remote Authentication
-- Dial In User Service (RADIUS) for an AD Connector directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DisableRadius.html>
module Network.AWS.Directory Service.DisableRadius
    (
    -- * Request
      DisableRadius
    -- ** Request constructor
    , disableRadius
    -- ** Request lenses
    , drDirectoryId

    -- * Response
    , DisableRadiusResponse
    -- ** Response constructor
    , disableRadiusResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Directory Service.Types

-- | /See:/ 'disableRadius' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drDirectoryId'
newtype DisableRadius = DisableRadius'{_drDirectoryId :: Text} deriving (Eq, Read, Show)

-- | 'DisableRadius' smart constructor.
disableRadius :: Text -> DisableRadius
disableRadius pDirectoryId = DisableRadius'{_drDirectoryId = pDirectoryId};

-- | The identifier of the directory to disable MFA for.
drDirectoryId :: Lens' DisableRadius Text
drDirectoryId = lens _drDirectoryId (\ s a -> s{_drDirectoryId = a});

instance AWSRequest DisableRadius where
        type Sv DisableRadius = Directory Service
        type Rs DisableRadius = DisableRadiusResponse
        request = postJSON
        response = receiveNull DisableRadiusResponse'

instance ToHeaders DisableRadius where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DisableRadius" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableRadius where
        toJSON DisableRadius'{..}
          = object ["DirectoryId" .= _drDirectoryId]

instance ToPath DisableRadius where
        toPath = const "/"

instance ToQuery DisableRadius where
        toQuery = const mempty

-- | /See:/ 'disableRadiusResponse' smart constructor.
data DisableRadiusResponse = DisableRadiusResponse' deriving (Eq, Read, Show)

-- | 'DisableRadiusResponse' smart constructor.
disableRadiusResponse :: DisableRadiusResponse
disableRadiusResponse = DisableRadiusResponse';
