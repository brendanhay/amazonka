{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.GetDirectoryLimits
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Obtains directory limit information for the current region.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_GetDirectoryLimits.html>
module Network.AWS.DirectoryService.GetDirectoryLimits
    (
    -- * Request
      GetDirectoryLimits
    -- ** Request constructor
    , getDirectoryLimits

    -- * Response
    , GetDirectoryLimitsResponse
    -- ** Response constructor
    , getDirectoryLimitsResponse
    -- ** Response lenses
    , gdlrsDirectoryLimits
    , gdlrsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the GetDirectoryLimits operation.
--
-- /See:/ 'getDirectoryLimits' smart constructor.
data GetDirectoryLimits =
    GetDirectoryLimits'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDirectoryLimits' smart constructor.
getDirectoryLimits :: GetDirectoryLimits
getDirectoryLimits = GetDirectoryLimits'

instance AWSRequest GetDirectoryLimits where
        type Sv GetDirectoryLimits = DirectoryService
        type Rs GetDirectoryLimits =
             GetDirectoryLimitsResponse
        request = postJSON "GetDirectoryLimits"
        response
          = receiveJSON
              (\ s h x ->
                 GetDirectoryLimitsResponse' <$>
                   (x .?> "DirectoryLimits") <*> (pure (fromEnum s)))

instance ToHeaders GetDirectoryLimits where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.GetDirectoryLimits" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDirectoryLimits where
        toJSON = const (Object mempty)

instance ToPath GetDirectoryLimits where
        toPath = const "/"

instance ToQuery GetDirectoryLimits where
        toQuery = const mempty

-- | Contains the results of the GetDirectoryLimits operation.
--
-- /See:/ 'getDirectoryLimitsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdlrsDirectoryLimits'
--
-- * 'gdlrsStatus'
data GetDirectoryLimitsResponse = GetDirectoryLimitsResponse'
    { _gdlrsDirectoryLimits :: !(Maybe DirectoryLimits)
    , _gdlrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDirectoryLimitsResponse' smart constructor.
getDirectoryLimitsResponse :: Int -> GetDirectoryLimitsResponse
getDirectoryLimitsResponse pStatus_ =
    GetDirectoryLimitsResponse'
    { _gdlrsDirectoryLimits = Nothing
    , _gdlrsStatus = pStatus_
    }

-- | A DirectoryLimits object that contains the directory limits for the
-- current region.
gdlrsDirectoryLimits :: Lens' GetDirectoryLimitsResponse (Maybe DirectoryLimits)
gdlrsDirectoryLimits = lens _gdlrsDirectoryLimits (\ s a -> s{_gdlrsDirectoryLimits = a});

-- | FIXME: Undocumented member.
gdlrsStatus :: Lens' GetDirectoryLimitsResponse Int
gdlrsStatus = lens _gdlrsStatus (\ s a -> s{_gdlrsStatus = a});
