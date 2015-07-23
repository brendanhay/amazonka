{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.ListDeploymentConfigs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment configurations with the applicable IAM user or AWS
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
    , ldcrqNextToken

    -- * Response
    , ListDeploymentConfigsResponse
    -- ** Response constructor
    , listDeploymentConfigsResponse
    -- ** Response lenses
    , ldcrsNextToken
    , ldcrsDeploymentConfigsList
    , ldcrsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list deployment configurations operation.
--
-- /See:/ 'listDeploymentConfigs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldcrqNextToken'
newtype ListDeploymentConfigs = ListDeploymentConfigs'
    { _ldcrqNextToken :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDeploymentConfigs' smart constructor.
listDeploymentConfigs :: ListDeploymentConfigs
listDeploymentConfigs =
    ListDeploymentConfigs'
    { _ldcrqNextToken = Nothing
    }

-- | An identifier that was returned from the previous list deployment
-- configurations call, which can be used to return the next set of
-- deployment configurations in the list.
ldcrqNextToken :: Lens' ListDeploymentConfigs (Maybe Text)
ldcrqNextToken = lens _ldcrqNextToken (\ s a -> s{_ldcrqNextToken = a});

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
          = object ["nextToken" .= _ldcrqNextToken]

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
-- * 'ldcrsNextToken'
--
-- * 'ldcrsDeploymentConfigsList'
--
-- * 'ldcrsStatus'
data ListDeploymentConfigsResponse = ListDeploymentConfigsResponse'
    { _ldcrsNextToken             :: !(Maybe Text)
    , _ldcrsDeploymentConfigsList :: !(Maybe [Text])
    , _ldcrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDeploymentConfigsResponse' smart constructor.
listDeploymentConfigsResponse :: Int -> ListDeploymentConfigsResponse
listDeploymentConfigsResponse pStatus_ =
    ListDeploymentConfigsResponse'
    { _ldcrsNextToken = Nothing
    , _ldcrsDeploymentConfigsList = Nothing
    , _ldcrsStatus = pStatus_
    }

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployment configurations call to return the next set of deployment
-- configurations in the list.
ldcrsNextToken :: Lens' ListDeploymentConfigsResponse (Maybe Text)
ldcrsNextToken = lens _ldcrsNextToken (\ s a -> s{_ldcrsNextToken = a});

-- | A list of deployment configurations, including the built-in
-- configurations such as CodeDeployDefault.OneAtATime.
ldcrsDeploymentConfigsList :: Lens' ListDeploymentConfigsResponse [Text]
ldcrsDeploymentConfigsList = lens _ldcrsDeploymentConfigsList (\ s a -> s{_ldcrsDeploymentConfigsList = a}) . _Default;

-- | FIXME: Undocumented member.
ldcrsStatus :: Lens' ListDeploymentConfigsResponse Int
ldcrsStatus = lens _ldcrsStatus (\ s a -> s{_ldcrsStatus = a});
