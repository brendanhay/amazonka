{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CodeDeploy.ListDeploymentConfigs
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

-- | Lists the deployment configurations with the applicable IAM user or AWS
-- account.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListDeploymentConfigs.html>
module Network.AWS.CodeDeploy.ListDeploymentConfigs
    (
    -- * Request
      ListDeploymentConfigs
    -- ** Request constructor
    , listDeploymentConfigs
    -- ** Request lenses
    , ldcNextToken

    -- * Response
    , ListDeploymentConfigsResponse
    -- ** Response constructor
    , listDeploymentConfigsResponse
    -- ** Response lenses
    , ldcrNextToken
    , ldcrDeploymentConfigsList
    , ldcrStatusCode
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a list deployment configurations operation.
--
-- /See:/ 'listDeploymentConfigs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldcNextToken'
newtype ListDeploymentConfigs = ListDeploymentConfigs'{_ldcNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'ListDeploymentConfigs' smart constructor.
listDeploymentConfigs :: ListDeploymentConfigs
listDeploymentConfigs = ListDeploymentConfigs'{_ldcNextToken = Nothing};

-- | An identifier that was returned from the previous list deployment
-- configurations call, which can be used to return the next set of
-- deployment configurations in the list.
ldcNextToken :: Lens' ListDeploymentConfigs (Maybe Text)
ldcNextToken = lens _ldcNextToken (\ s a -> s{_ldcNextToken = a});

instance AWSRequest ListDeploymentConfigs where
        type Sv ListDeploymentConfigs = CodeDeploy
        type Rs ListDeploymentConfigs =
             ListDeploymentConfigsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDeploymentConfigsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "deploymentConfigsList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListDeploymentConfigs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ListDeploymentConfigs" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDeploymentConfigs where
        toJSON ListDeploymentConfigs'{..}
          = object ["nextToken" .= _ldcNextToken]

instance ToPath ListDeploymentConfigs where
        toPath = const "/"

instance ToQuery ListDeploymentConfigs where
        toQuery = const mempty

-- | Represents the output of a list deployment configurations operation.
--
-- /See:/ 'listDeploymentConfigsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldcrNextToken'
--
-- * 'ldcrDeploymentConfigsList'
--
-- * 'ldcrStatusCode'
data ListDeploymentConfigsResponse = ListDeploymentConfigsResponse'{_ldcrNextToken :: Maybe Text, _ldcrDeploymentConfigsList :: Maybe [Text], _ldcrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'ListDeploymentConfigsResponse' smart constructor.
listDeploymentConfigsResponse :: Int -> ListDeploymentConfigsResponse
listDeploymentConfigsResponse pStatusCode = ListDeploymentConfigsResponse'{_ldcrNextToken = Nothing, _ldcrDeploymentConfigsList = Nothing, _ldcrStatusCode = pStatusCode};

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployment configurations call to return the next set of deployment
-- configurations in the list.
ldcrNextToken :: Lens' ListDeploymentConfigsResponse (Maybe Text)
ldcrNextToken = lens _ldcrNextToken (\ s a -> s{_ldcrNextToken = a});

-- | A list of deployment configurations, including the built-in
-- configurations such as CodeDeployDefault.OneAtATime.
ldcrDeploymentConfigsList :: Lens' ListDeploymentConfigsResponse [Text]
ldcrDeploymentConfigsList = lens _ldcrDeploymentConfigsList (\ s a -> s{_ldcrDeploymentConfigsList = a}) . _Default;

-- | FIXME: Undocumented member.
ldcrStatusCode :: Lens' ListDeploymentConfigsResponse Int
ldcrStatusCode = lens _ldcrStatusCode (\ s a -> s{_ldcrStatusCode = a});
