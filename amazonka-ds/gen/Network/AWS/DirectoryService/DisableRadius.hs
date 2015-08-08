{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DisableRadius
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables multi-factor authentication (MFA) with Remote Authentication
-- Dial In User Service (RADIUS) for an AD Connector directory.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DisableRadius.html AWS API Reference> for DisableRadius.
module Network.AWS.DirectoryService.DisableRadius
    (
    -- * Creating a Request
      DisableRadius
    , disableRadius
    -- * Request Lenses
    , drDirectoryId

    -- * Destructuring the Response
    , DisableRadiusResponse
    , disableRadiusResponse
    -- * Response Lenses
    , drrsStatus
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
disableRadius pDirectoryId_ =
    DisableRadius'
    { _drDirectoryId = pDirectoryId_
    }

-- | The identifier of the directory to disable MFA for.
drDirectoryId :: Lens' DisableRadius Text
drDirectoryId = lens _drDirectoryId (\ s a -> s{_drDirectoryId = a});

instance AWSRequest DisableRadius where
        type Sv DisableRadius = DirectoryService
        type Rs DisableRadius = DisableRadiusResponse
        request = postJSON
        response
          = receiveEmpty
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
-- * 'drrsStatus'
newtype DisableRadiusResponse = DisableRadiusResponse'
    { _drrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableRadiusResponse' smart constructor.
disableRadiusResponse :: Int -> DisableRadiusResponse
disableRadiusResponse pStatus_ =
    DisableRadiusResponse'
    { _drrsStatus = pStatus_
    }

-- | Undocumented member.
drrsStatus :: Lens' DisableRadiusResponse Int
drrsStatus = lens _drrsStatus (\ s a -> s{_drrsStatus = a});
