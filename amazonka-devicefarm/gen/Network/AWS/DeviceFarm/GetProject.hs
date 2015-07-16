{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetProject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a project.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetProject.html>
module Network.AWS.DeviceFarm.GetProject
    (
    -- * Request
      GetProject
    -- ** Request constructor
    , getProject
    -- ** Request lenses
    , gpArn

    -- * Response
    , GetProjectResponse
    -- ** Response constructor
    , getProjectResponse
    -- ** Response lenses
    , gprProject
    , gprStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get project operation.
--
-- /See:/ 'getProject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpArn'
newtype GetProject = GetProject'
    { _gpArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetProject' smart constructor.
getProject :: Text -> GetProject
getProject pArn =
    GetProject'
    { _gpArn = pArn
    }

-- | The project\'s ARN.
gpArn :: Lens' GetProject Text
gpArn = lens _gpArn (\ s a -> s{_gpArn = a});

instance AWSRequest GetProject where
        type Sv GetProject = DeviceFarm
        type Rs GetProject = GetProjectResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetProjectResponse' <$>
                   (x .?> "project") <*> (pure (fromEnum s)))

instance ToHeaders GetProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetProject" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetProject where
        toJSON GetProject'{..} = object ["arn" .= _gpArn]

instance ToPath GetProject where
        toPath = const "/"

instance ToQuery GetProject where
        toQuery = const mempty

-- | Represents the result of a get project request.
--
-- /See:/ 'getProjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gprProject'
--
-- * 'gprStatus'
data GetProjectResponse = GetProjectResponse'
    { _gprProject :: !(Maybe Project)
    , _gprStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetProjectResponse' smart constructor.
getProjectResponse :: Int -> GetProjectResponse
getProjectResponse pStatus =
    GetProjectResponse'
    { _gprProject = Nothing
    , _gprStatus = pStatus
    }

-- | FIXME: Undocumented member.
gprProject :: Lens' GetProjectResponse (Maybe Project)
gprProject = lens _gprProject (\ s a -> s{_gprProject = a});

-- | FIXME: Undocumented member.
gprStatus :: Lens' GetProjectResponse Int
gprStatus = lens _gprStatus (\ s a -> s{_gprStatus = a});
