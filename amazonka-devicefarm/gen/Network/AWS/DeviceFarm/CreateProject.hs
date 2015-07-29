{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateProject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new project.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_CreateProject.html>
module Network.AWS.DeviceFarm.CreateProject
    (
    -- * Request
      CreateProject
    -- ** Request constructor
    , createProject
    -- ** Request lenses
    , cpName

    -- * Response
    , CreateProjectResponse
    -- ** Response constructor
    , createProjectResponse
    -- ** Response lenses
    , cprsProject
    , cprsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the create project operation.
--
-- /See:/ 'createProject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpName'
newtype CreateProject = CreateProject'
    { _cpName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateProject' smart constructor.
createProject :: Text -> CreateProject
createProject pName_ =
    CreateProject'
    { _cpName = pName_
    }

-- | The project\'s name.
cpName :: Lens' CreateProject Text
cpName = lens _cpName (\ s a -> s{_cpName = a});

instance AWSRequest CreateProject where
        type Sv CreateProject = DeviceFarm
        type Rs CreateProject = CreateProjectResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateProjectResponse' <$>
                   (x .?> "project") <*> (pure (fromEnum s)))

instance ToHeaders CreateProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.CreateProject" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateProject where
        toJSON CreateProject'{..}
          = object ["name" .= _cpName]

instance ToPath CreateProject where
        toPath = const mempty

instance ToQuery CreateProject where
        toQuery = const mempty

-- | Represents the result of a create project request.
--
-- /See:/ 'createProjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cprsProject'
--
-- * 'cprsStatus'
data CreateProjectResponse = CreateProjectResponse'
    { _cprsProject :: !(Maybe Project)
    , _cprsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateProjectResponse' smart constructor.
createProjectResponse :: Int -> CreateProjectResponse
createProjectResponse pStatus_ =
    CreateProjectResponse'
    { _cprsProject = Nothing
    , _cprsStatus = pStatus_
    }

-- | The newly created project.
cprsProject :: Lens' CreateProjectResponse (Maybe Project)
cprsProject = lens _cprsProject (\ s a -> s{_cprsProject = a});

-- | FIXME: Undocumented member.
cprsStatus :: Lens' CreateProjectResponse Int
cprsStatus = lens _cprsStatus (\ s a -> s{_cprsStatus = a});
