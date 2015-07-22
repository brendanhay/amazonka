{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , errqDirectoryId
    , errqRadiusSettings

    -- * Response
    , EnableRadiusResponse
    -- ** Response constructor
    , enableRadiusResponse
    -- ** Response lenses
    , errsStatus
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
-- * 'errqDirectoryId'
--
-- * 'errqRadiusSettings'
data EnableRadius = EnableRadius'
    { _errqDirectoryId    :: !Text
    , _errqRadiusSettings :: !RadiusSettings
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableRadius' smart constructor.
enableRadius :: Text -> RadiusSettings -> EnableRadius
enableRadius pDirectoryId pRadiusSettings =
    EnableRadius'
    { _errqDirectoryId = pDirectoryId
    , _errqRadiusSettings = pRadiusSettings
    }

-- | The identifier of the directory to enable MFA for.
errqDirectoryId :: Lens' EnableRadius Text
errqDirectoryId = lens _errqDirectoryId (\ s a -> s{_errqDirectoryId = a});

-- | A RadiusSettings object that contains information about the RADIUS
-- server.
errqRadiusSettings :: Lens' EnableRadius RadiusSettings
errqRadiusSettings = lens _errqRadiusSettings (\ s a -> s{_errqRadiusSettings = a});

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
              ["DirectoryId" .= _errqDirectoryId,
               "RadiusSettings" .= _errqRadiusSettings]

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
-- * 'errsStatus'
newtype EnableRadiusResponse = EnableRadiusResponse'
    { _errsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableRadiusResponse' smart constructor.
enableRadiusResponse :: Int -> EnableRadiusResponse
enableRadiusResponse pStatus =
    EnableRadiusResponse'
    { _errsStatus = pStatus
    }

-- | FIXME: Undocumented member.
errsStatus :: Lens' EnableRadiusResponse Int
errsStatus = lens _errsStatus (\ s a -> s{_errsStatus = a});
