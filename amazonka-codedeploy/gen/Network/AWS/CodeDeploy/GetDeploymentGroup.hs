{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.GetDeploymentGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Gets information about a deployment group.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetDeploymentGroup.html>
module Network.AWS.CodeDeploy.GetDeploymentGroup
    (
    -- * Request
      GetDeploymentGroup
    -- ** Request constructor
    , getDeploymentGroup
    -- ** Request lenses
    , gdgApplicationName
    , gdgDeploymentGroupName

    -- * Response
    , GetDeploymentGroupResponse
    -- ** Response constructor
    , getDeploymentGroupResponse
    -- ** Response lenses
    , gdgrDeploymentGroupInfo
    , gdgrStatusCode
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a get deployment group operation.
--
-- /See:/ 'getDeploymentGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdgApplicationName'
--
-- * 'gdgDeploymentGroupName'
data GetDeploymentGroup = GetDeploymentGroup'{_gdgApplicationName :: Text, _gdgDeploymentGroupName :: Text} deriving (Eq, Read, Show)

-- | 'GetDeploymentGroup' smart constructor.
getDeploymentGroup :: Text -> Text -> GetDeploymentGroup
getDeploymentGroup pApplicationName pDeploymentGroupName = GetDeploymentGroup'{_gdgApplicationName = pApplicationName, _gdgDeploymentGroupName = pDeploymentGroupName};

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
gdgApplicationName :: Lens' GetDeploymentGroup Text
gdgApplicationName = lens _gdgApplicationName (\ s a -> s{_gdgApplicationName = a});

-- | The name of an existing deployment group for the specified application.
gdgDeploymentGroupName :: Lens' GetDeploymentGroup Text
gdgDeploymentGroupName = lens _gdgDeploymentGroupName (\ s a -> s{_gdgDeploymentGroupName = a});

instance AWSRequest GetDeploymentGroup where
        type Sv GetDeploymentGroup = CodeDeploy
        type Rs GetDeploymentGroup =
             GetDeploymentGroupResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetDeploymentGroupResponse' <$>
                   (x .?> "deploymentGroupInfo") <*>
                     (pure (fromEnum s)))

instance ToHeaders GetDeploymentGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetDeploymentGroup" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDeploymentGroup where
        toJSON GetDeploymentGroup'{..}
          = object
              ["applicationName" .= _gdgApplicationName,
               "deploymentGroupName" .= _gdgDeploymentGroupName]

instance ToPath GetDeploymentGroup where
        toPath = const "/"

instance ToQuery GetDeploymentGroup where
        toQuery = const mempty

-- | Represents the output of a get deployment group operation.
--
-- /See:/ 'getDeploymentGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdgrDeploymentGroupInfo'
--
-- * 'gdgrStatusCode'
data GetDeploymentGroupResponse = GetDeploymentGroupResponse'{_gdgrDeploymentGroupInfo :: Maybe DeploymentGroupInfo, _gdgrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'GetDeploymentGroupResponse' smart constructor.
getDeploymentGroupResponse :: Int -> GetDeploymentGroupResponse
getDeploymentGroupResponse pStatusCode = GetDeploymentGroupResponse'{_gdgrDeploymentGroupInfo = Nothing, _gdgrStatusCode = pStatusCode};

-- | Information about the deployment group.
gdgrDeploymentGroupInfo :: Lens' GetDeploymentGroupResponse (Maybe DeploymentGroupInfo)
gdgrDeploymentGroupInfo = lens _gdgrDeploymentGroupInfo (\ s a -> s{_gdgrDeploymentGroupInfo = a});

-- | FIXME: Undocumented member.
gdgrStatusCode :: Lens' GetDeploymentGroupResponse Int
gdgrStatusCode = lens _gdgrStatusCode (\ s a -> s{_gdgrStatusCode = a});
