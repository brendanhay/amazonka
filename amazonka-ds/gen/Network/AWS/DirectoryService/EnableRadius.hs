{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.EnableRadius
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Enables multi-factor authentication (MFA) with Remote Authentication
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
    -- ** Response lenses
    , errStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the EnableRadius operation.
--
-- /See:/ 'enableRadius' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'erDirectoryId'
--
-- * 'erRadiusSettings'
data EnableRadius = EnableRadius'
    { _erDirectoryId    :: !Text
    , _erRadiusSettings :: !RadiusSettings
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableRadius' smart constructor.
enableRadius :: Text -> RadiusSettings -> EnableRadius
enableRadius pDirectoryId pRadiusSettings =
    EnableRadius'
    { _erDirectoryId = pDirectoryId
    , _erRadiusSettings = pRadiusSettings
    }

-- | The identifier of the directory to enable MFA for.
erDirectoryId :: Lens' EnableRadius Text
erDirectoryId = lens _erDirectoryId (\ s a -> s{_erDirectoryId = a});

-- | A RadiusSettings object that contains information about the RADIUS
-- server.
erRadiusSettings :: Lens' EnableRadius RadiusSettings
erRadiusSettings = lens _erRadiusSettings (\ s a -> s{_erRadiusSettings = a});

instance AWSRequest EnableRadius where
        type Sv EnableRadius = DirectoryService
        type Rs EnableRadius = EnableRadiusResponse
        request = postJSON
        response
          = receiveJSON
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
              ["DirectoryId" .= _erDirectoryId,
               "RadiusSettings" .= _erRadiusSettings]

instance ToPath EnableRadius where
        toPath = const "/"

instance ToQuery EnableRadius where
        toQuery = const mempty

-- | Contains the results of the EnableRadius operation.
--
-- /See:/ 'enableRadiusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'errStatus'
newtype EnableRadiusResponse = EnableRadiusResponse'
    { _errStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableRadiusResponse' smart constructor.
enableRadiusResponse :: Int -> EnableRadiusResponse
enableRadiusResponse pStatus =
    EnableRadiusResponse'
    { _errStatus = pStatus
    }

-- | FIXME: Undocumented member.
errStatus :: Lens' EnableRadiusResponse Int
errStatus = lens _errStatus (\ s a -> s{_errStatus = a});
