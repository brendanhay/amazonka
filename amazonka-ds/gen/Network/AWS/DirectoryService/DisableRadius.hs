{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.DirectoryService.DisableRadius
-- Copyright   : (c) 2013-2015 Brendan Hay
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
module Network.AWS.DirectoryService.DisableRadius
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
    -- ** Response lenses
    , drrStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DisableRadius operation.
--
-- /See:/ 'disableRadius' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drDirectoryId'
newtype DisableRadius = DisableRadius'
    { _drDirectoryId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableRadius' smart constructor.
disableRadius :: Text -> DisableRadius
disableRadius pDirectoryId =
    DisableRadius'
    { _drDirectoryId = pDirectoryId
    }

-- | The identifier of the directory to disable MFA for.
drDirectoryId :: Lens' DisableRadius Text
drDirectoryId = lens _drDirectoryId (\ s a -> s{_drDirectoryId = a});

instance AWSRequest DisableRadius where
        type Sv DisableRadius = DirectoryService
        type Rs DisableRadius = DisableRadiusResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DisableRadiusResponse' <$> (pure (fromEnum s)))

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

-- | Contains the results of the DisableRadius operation.
--
-- /See:/ 'disableRadiusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drrStatus'
newtype DisableRadiusResponse = DisableRadiusResponse'
    { _drrStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableRadiusResponse' smart constructor.
disableRadiusResponse :: Int -> DisableRadiusResponse
disableRadiusResponse pStatus =
    DisableRadiusResponse'
    { _drrStatus = pStatus
    }

-- | FIXME: Undocumented member.
drrStatus :: Lens' DisableRadiusResponse Int
drrStatus = lens _drrStatus (\ s a -> s{_drrStatus = a});
