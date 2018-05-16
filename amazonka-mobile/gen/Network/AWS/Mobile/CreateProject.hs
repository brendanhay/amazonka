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
-- Module      : Network.AWS.Mobile.CreateProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Mobile Hub project.
--
--
module Network.AWS.Mobile.CreateProject
    (
    -- * Creating a Request
      createProject
    , CreateProject
    -- * Request Lenses
    , cpContents
    , cpName
    , cpRegion
    , cpSnapshotId

    -- * Destructuring the Response
    , createProjectResponse
    , CreateProjectResponse
    -- * Response Lenses
    , cprsDetails
    , cprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Mobile.Types
import Network.AWS.Mobile.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure used to request a project be created.
--
--
--
-- /See:/ 'createProject' smart constructor.
data CreateProject = CreateProject'
  { _cpContents   :: !(Maybe ByteString)
  , _cpName       :: !(Maybe Text)
  , _cpRegion     :: !(Maybe Text)
  , _cpSnapshotId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpContents' - ZIP or YAML file which contains configuration settings to be used when creating the project. This may be the contents of the file downloaded from the URL provided in an export project operation.
--
-- * 'cpName' - Name of the project.
--
-- * 'cpRegion' - Default region where project resources should be created.
--
-- * 'cpSnapshotId' - Unique identifier for an exported snapshot of project configuration. This snapshot identifier is included in the share URL when a project is exported.
createProject
    :: CreateProject
createProject =
  CreateProject'
    { _cpContents = Nothing
    , _cpName = Nothing
    , _cpRegion = Nothing
    , _cpSnapshotId = Nothing
    }


-- | ZIP or YAML file which contains configuration settings to be used when creating the project. This may be the contents of the file downloaded from the URL provided in an export project operation.
cpContents :: Lens' CreateProject (Maybe ByteString)
cpContents = lens _cpContents (\ s a -> s{_cpContents = a})

-- | Name of the project.
cpName :: Lens' CreateProject (Maybe Text)
cpName = lens _cpName (\ s a -> s{_cpName = a})

-- | Default region where project resources should be created.
cpRegion :: Lens' CreateProject (Maybe Text)
cpRegion = lens _cpRegion (\ s a -> s{_cpRegion = a})

-- | Unique identifier for an exported snapshot of project configuration. This snapshot identifier is included in the share URL when a project is exported.
cpSnapshotId :: Lens' CreateProject (Maybe Text)
cpSnapshotId = lens _cpSnapshotId (\ s a -> s{_cpSnapshotId = a})

instance AWSRequest CreateProject where
        type Rs CreateProject = CreateProjectResponse
        request = postBody mobile
        response
          = receiveJSON
              (\ s h x ->
                 CreateProjectResponse' <$>
                   (x .?> "details") <*> (pure (fromEnum s)))

instance Hashable CreateProject where

instance NFData CreateProject where

instance ToBody CreateProject where
        toBody = toBody . _cpContents

instance ToHeaders CreateProject where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath CreateProject where
        toPath = const "/projects"

instance ToQuery CreateProject where
        toQuery CreateProject'{..}
          = mconcat
              ["name" =: _cpName, "region" =: _cpRegion,
               "snapshotId" =: _cpSnapshotId]

-- | Result structure used in response to a request to create a project.
--
--
--
-- /See:/ 'createProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { _cprsDetails        :: !(Maybe ProjectDetails)
  , _cprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsDetails' - Detailed information about the created AWS Mobile Hub project.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createProjectResponse
    :: Int -- ^ 'cprsResponseStatus'
    -> CreateProjectResponse
createProjectResponse pResponseStatus_ =
  CreateProjectResponse'
    {_cprsDetails = Nothing, _cprsResponseStatus = pResponseStatus_}


-- | Detailed information about the created AWS Mobile Hub project.
cprsDetails :: Lens' CreateProjectResponse (Maybe ProjectDetails)
cprsDetails = lens _cprsDetails (\ s a -> s{_cprsDetails = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreateProjectResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\ s a -> s{_cprsResponseStatus = a})

instance NFData CreateProjectResponse where
