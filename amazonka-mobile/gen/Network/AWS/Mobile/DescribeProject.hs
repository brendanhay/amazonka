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
-- Module      : Network.AWS.Mobile.DescribeProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a project in AWS Mobile Hub.
--
--
module Network.AWS.Mobile.DescribeProject
    (
    -- * Creating a Request
      describeProject
    , DescribeProject
    -- * Request Lenses
    , dSyncFromResources
    , dProjectId

    -- * Destructuring the Response
    , describeProjectResponse
    , DescribeProjectResponse
    -- * Response Lenses
    , drsDetails
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Mobile.Types
import Network.AWS.Mobile.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure used to request details about a project.
--
--
--
-- /See:/ 'describeProject' smart constructor.
data DescribeProject = DescribeProject'
  { _dSyncFromResources :: !(Maybe Bool)
  , _dProjectId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dSyncFromResources' - If set to true, causes AWS Mobile Hub to synchronize information from other services, e.g., update state of AWS CloudFormation stacks in the AWS Mobile Hub project.
--
-- * 'dProjectId' - Unique project identifier.
describeProject
    :: Text -- ^ 'dProjectId'
    -> DescribeProject
describeProject pProjectId_ =
  DescribeProject' {_dSyncFromResources = Nothing, _dProjectId = pProjectId_}


-- | If set to true, causes AWS Mobile Hub to synchronize information from other services, e.g., update state of AWS CloudFormation stacks in the AWS Mobile Hub project.
dSyncFromResources :: Lens' DescribeProject (Maybe Bool)
dSyncFromResources = lens _dSyncFromResources (\ s a -> s{_dSyncFromResources = a})

-- | Unique project identifier.
dProjectId :: Lens' DescribeProject Text
dProjectId = lens _dProjectId (\ s a -> s{_dProjectId = a})

instance AWSRequest DescribeProject where
        type Rs DescribeProject = DescribeProjectResponse
        request = get mobile
        response
          = receiveJSON
              (\ s h x ->
                 DescribeProjectResponse' <$>
                   (x .?> "details") <*> (pure (fromEnum s)))

instance Hashable DescribeProject where

instance NFData DescribeProject where

instance ToHeaders DescribeProject where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeProject where
        toPath = const "/project"

instance ToQuery DescribeProject where
        toQuery DescribeProject'{..}
          = mconcat
              ["syncFromResources" =: _dSyncFromResources,
               "projectId" =: _dProjectId]

-- | Result structure used for requests of project details.
--
--
--
-- /See:/ 'describeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { _drsDetails        :: !(Maybe ProjectDetails)
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsDetails' - Undocumented member.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeProjectResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeProjectResponse
describeProjectResponse pResponseStatus_ =
  DescribeProjectResponse'
    {_drsDetails = Nothing, _drsResponseStatus = pResponseStatus_}


-- | Undocumented member.
drsDetails :: Lens' DescribeProjectResponse (Maybe ProjectDetails)
drsDetails = lens _drsDetails (\ s a -> s{_drsDetails = a})

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeProjectResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeProjectResponse where
