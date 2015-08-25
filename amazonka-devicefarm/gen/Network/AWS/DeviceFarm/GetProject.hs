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
-- Module      : Network.AWS.DeviceFarm.GetProject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a project.
--
-- /See:/ <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetProject.html AWS API Reference> for GetProject.
module Network.AWS.DeviceFarm.GetProject
    (
    -- * Creating a Request
      getProject
    , GetProject
    -- * Request Lenses
    , gpArn

    -- * Destructuring the Response
    , getProjectResponse
    , GetProjectResponse
    -- * Response Lenses
    , gprsProject
    , gprsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.DeviceFarm.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get project operation.
--
-- /See:/ 'getProject' smart constructor.
newtype GetProject = GetProject'
    { _gpArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpArn'
getProject
    :: Text -- ^ 'gpArn'
    -> GetProject
getProject pArn_ =
    GetProject'
    { _gpArn = pArn_
    }

-- | The project\'s ARN.
gpArn :: Lens' GetProject Text
gpArn = lens _gpArn (\ s a -> s{_gpArn = a});

instance AWSRequest GetProject where
        type Rs GetProject = GetProjectResponse
        request = postJSON deviceFarm
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
        toJSON GetProject'{..}
          = object (catMaybes [Just ("arn" .= _gpArn)])

instance ToPath GetProject where
        toPath = const "/"

instance ToQuery GetProject where
        toQuery = const mempty

-- | Represents the result of a get project request.
--
-- /See:/ 'getProjectResponse' smart constructor.
data GetProjectResponse = GetProjectResponse'
    { _gprsProject :: !(Maybe Project)
    , _gprsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsProject'
--
-- * 'gprsStatus'
getProjectResponse
    :: Int -- ^ 'gprsStatus'
    -> GetProjectResponse
getProjectResponse pStatus_ =
    GetProjectResponse'
    { _gprsProject = Nothing
    , _gprsStatus = pStatus_
    }

-- | Undocumented member.
gprsProject :: Lens' GetProjectResponse (Maybe Project)
gprsProject = lens _gprsProject (\ s a -> s{_gprsProject = a});

-- | The response status code.
gprsStatus :: Lens' GetProjectResponse Int
gprsStatus = lens _gprsStatus (\ s a -> s{_gprsStatus = a});
