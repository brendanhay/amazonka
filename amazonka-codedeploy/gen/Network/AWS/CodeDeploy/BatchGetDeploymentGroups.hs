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
-- Module      : Network.AWS.CodeDeploy.BatchGetDeploymentGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more deployment groups.
--
--
module Network.AWS.CodeDeploy.BatchGetDeploymentGroups
    (
    -- * Creating a Request
      batchGetDeploymentGroups
    , BatchGetDeploymentGroups
    -- * Request Lenses
    , bgdgApplicationName
    , bgdgDeploymentGroupNames

    -- * Destructuring the Response
    , batchGetDeploymentGroupsResponse
    , BatchGetDeploymentGroupsResponse
    -- * Response Lenses
    , bgdgrsDeploymentGroupsInfo
    , bgdgrsErrorMessage
    , bgdgrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a BatchGetDeploymentGroups operation.
--
--
--
-- /See:/ 'batchGetDeploymentGroups' smart constructor.
data BatchGetDeploymentGroups = BatchGetDeploymentGroups'
  { _bgdgApplicationName      :: !Text
  , _bgdgDeploymentGroupNames :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetDeploymentGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgdgApplicationName' - The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
--
-- * 'bgdgDeploymentGroupNames' - The deployment groups' names.
batchGetDeploymentGroups
    :: Text -- ^ 'bgdgApplicationName'
    -> BatchGetDeploymentGroups
batchGetDeploymentGroups pApplicationName_ =
  BatchGetDeploymentGroups'
    { _bgdgApplicationName = pApplicationName_
    , _bgdgDeploymentGroupNames = mempty
    }


-- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
bgdgApplicationName :: Lens' BatchGetDeploymentGroups Text
bgdgApplicationName = lens _bgdgApplicationName (\ s a -> s{_bgdgApplicationName = a})

-- | The deployment groups' names.
bgdgDeploymentGroupNames :: Lens' BatchGetDeploymentGroups [Text]
bgdgDeploymentGroupNames = lens _bgdgDeploymentGroupNames (\ s a -> s{_bgdgDeploymentGroupNames = a}) . _Coerce

instance AWSRequest BatchGetDeploymentGroups where
        type Rs BatchGetDeploymentGroups =
             BatchGetDeploymentGroupsResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetDeploymentGroupsResponse' <$>
                   (x .?> "deploymentGroupsInfo" .!@ mempty) <*>
                     (x .?> "errorMessage")
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetDeploymentGroups where

instance NFData BatchGetDeploymentGroups where

instance ToHeaders BatchGetDeploymentGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.BatchGetDeploymentGroups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetDeploymentGroups where
        toJSON BatchGetDeploymentGroups'{..}
          = object
              (catMaybes
                 [Just ("applicationName" .= _bgdgApplicationName),
                  Just
                    ("deploymentGroupNames" .=
                       _bgdgDeploymentGroupNames)])

instance ToPath BatchGetDeploymentGroups where
        toPath = const "/"

instance ToQuery BatchGetDeploymentGroups where
        toQuery = const mempty

-- | Represents the output of a BatchGetDeploymentGroups operation.
--
--
--
-- /See:/ 'batchGetDeploymentGroupsResponse' smart constructor.
data BatchGetDeploymentGroupsResponse = BatchGetDeploymentGroupsResponse'
  { _bgdgrsDeploymentGroupsInfo :: !(Maybe [DeploymentGroupInfo])
  , _bgdgrsErrorMessage         :: !(Maybe Text)
  , _bgdgrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetDeploymentGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgdgrsDeploymentGroupsInfo' - Information about the deployment groups.
--
-- * 'bgdgrsErrorMessage' - Information about errors that may have occurred during the API call.
--
-- * 'bgdgrsResponseStatus' - -- | The response status code.
batchGetDeploymentGroupsResponse
    :: Int -- ^ 'bgdgrsResponseStatus'
    -> BatchGetDeploymentGroupsResponse
batchGetDeploymentGroupsResponse pResponseStatus_ =
  BatchGetDeploymentGroupsResponse'
    { _bgdgrsDeploymentGroupsInfo = Nothing
    , _bgdgrsErrorMessage = Nothing
    , _bgdgrsResponseStatus = pResponseStatus_
    }


-- | Information about the deployment groups.
bgdgrsDeploymentGroupsInfo :: Lens' BatchGetDeploymentGroupsResponse [DeploymentGroupInfo]
bgdgrsDeploymentGroupsInfo = lens _bgdgrsDeploymentGroupsInfo (\ s a -> s{_bgdgrsDeploymentGroupsInfo = a}) . _Default . _Coerce

-- | Information about errors that may have occurred during the API call.
bgdgrsErrorMessage :: Lens' BatchGetDeploymentGroupsResponse (Maybe Text)
bgdgrsErrorMessage = lens _bgdgrsErrorMessage (\ s a -> s{_bgdgrsErrorMessage = a})

-- | -- | The response status code.
bgdgrsResponseStatus :: Lens' BatchGetDeploymentGroupsResponse Int
bgdgrsResponseStatus = lens _bgdgrsResponseStatus (\ s a -> s{_bgdgrsResponseStatus = a})

instance NFData BatchGetDeploymentGroupsResponse
         where
