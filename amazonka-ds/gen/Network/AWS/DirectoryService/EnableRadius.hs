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
-- Module      : Network.AWS.DirectoryService.EnableRadius
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables multi-factor authentication (MFA) with Remote Authentication
-- Dial In User Service (RADIUS) for an AD Connector directory.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_EnableRadius.html AWS API Reference> for EnableRadius.
module Network.AWS.DirectoryService.EnableRadius
    (
    -- * Creating a Request
      enableRadius
    , EnableRadius
    -- * Request Lenses
    , erDirectoryId
    , erRadiusSettings

    -- * Destructuring the Response
    , enableRadiusResponse
    , EnableRadiusResponse
    -- * Response Lenses
    , errsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.DirectoryService.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the EnableRadius operation.
--
-- /See:/ 'enableRadius' smart constructor.
data EnableRadius = EnableRadius'
    { _erDirectoryId    :: !Text
    , _erRadiusSettings :: !RadiusSettings
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableRadius' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erDirectoryId'
--
-- * 'erRadiusSettings'
enableRadius
    :: Text -- ^ 'erDirectoryId'
    -> RadiusSettings -- ^ 'erRadiusSettings'
    -> EnableRadius
enableRadius pDirectoryId_ pRadiusSettings_ =
    EnableRadius'
    { _erDirectoryId = pDirectoryId_
    , _erRadiusSettings = pRadiusSettings_
    }

-- | The identifier of the directory to enable MFA for.
erDirectoryId :: Lens' EnableRadius Text
erDirectoryId = lens _erDirectoryId (\ s a -> s{_erDirectoryId = a});

-- | A RadiusSettings object that contains information about the RADIUS
-- server.
erRadiusSettings :: Lens' EnableRadius RadiusSettings
erRadiusSettings = lens _erRadiusSettings (\ s a -> s{_erRadiusSettings = a});

instance AWSRequest EnableRadius where
        type Rs EnableRadius = EnableRadiusResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 EnableRadiusResponse' <$> (pure (fromEnum s)))

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
              (catMaybes
                 [Just ("DirectoryId" .= _erDirectoryId),
                  Just ("RadiusSettings" .= _erRadiusSettings)])

instance ToPath EnableRadius where
        toPath = const "/"

instance ToQuery EnableRadius where
        toQuery = const mempty

-- | Contains the results of the EnableRadius operation.
--
-- /See:/ 'enableRadiusResponse' smart constructor.
newtype EnableRadiusResponse = EnableRadiusResponse'
    { _errsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableRadiusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'errsStatus'
enableRadiusResponse
    :: Int -- ^ 'errsStatus'
    -> EnableRadiusResponse
enableRadiusResponse pStatus_ =
    EnableRadiusResponse'
    { _errsStatus = pStatus_
    }

-- | The response status code.
errsStatus :: Lens' EnableRadiusResponse Int
errsStatus = lens _errsStatus (\ s a -> s{_errsStatus = a});
