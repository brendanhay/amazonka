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
-- Module      : Network.AWS.CodeDeploy.ListDeploymentConfigs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment configurations with the applicable IAM user or AWS
-- account.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListDeploymentConfigs.html AWS API Reference> for ListDeploymentConfigs.
module Network.AWS.CodeDeploy.ListDeploymentConfigs
    (
    -- * Creating a Request
      listDeploymentConfigs
    , ListDeploymentConfigs
    -- * Request Lenses
    , ldcNextToken

    -- * Destructuring the Response
    , listDeploymentConfigsResponse
    , ListDeploymentConfigsResponse
    -- * Response Lenses
    , ldcrsNextToken
    , ldcrsDeploymentConfigsList
    , ldcrsResponseStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list deployment configurations operation.
--
-- /See:/ 'listDeploymentConfigs' smart constructor.
newtype ListDeploymentConfigs = ListDeploymentConfigs'
    { _ldcNextToken :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDeploymentConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldcNextToken'
listDeploymentConfigs
    :: ListDeploymentConfigs
listDeploymentConfigs =
    ListDeploymentConfigs'
    { _ldcNextToken = Nothing
    }

-- | An identifier that was returned from the previous list deployment
-- configurations call, which can be used to return the next set of
-- deployment configurations in the list.
ldcNextToken :: Lens' ListDeploymentConfigs (Maybe Text)
ldcNextToken = lens _ldcNextToken (\ s a -> s{_ldcNextToken = a});

instance AWSRequest ListDeploymentConfigs where
        type Rs ListDeploymentConfigs =
             ListDeploymentConfigsResponse
        request = postJSON codeDeploy
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
          = object
              (catMaybes [("nextToken" .=) <$> _ldcNextToken])

instance ToPath ListDeploymentConfigs where
        toPath = const "/"

instance ToQuery ListDeploymentConfigs where
        toQuery = const mempty

-- | Represents the output of a list deployment configurations operation.
--
-- /See:/ 'listDeploymentConfigsResponse' smart constructor.
data ListDeploymentConfigsResponse = ListDeploymentConfigsResponse'
    { _ldcrsNextToken             :: !(Maybe Text)
    , _ldcrsDeploymentConfigsList :: !(Maybe [Text])
    , _ldcrsResponseStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListDeploymentConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldcrsNextToken'
--
-- * 'ldcrsDeploymentConfigsList'
--
-- * 'ldcrsResponseStatus'
listDeploymentConfigsResponse
    :: Int -- ^ 'ldcrsResponseStatus'
    -> ListDeploymentConfigsResponse
listDeploymentConfigsResponse pResponseStatus_ =
    ListDeploymentConfigsResponse'
    { _ldcrsNextToken = Nothing
    , _ldcrsDeploymentConfigsList = Nothing
    , _ldcrsResponseStatus = pResponseStatus_
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
ldcrsDeploymentConfigsList = lens _ldcrsDeploymentConfigsList (\ s a -> s{_ldcrsDeploymentConfigsList = a}) . _Default . _Coerce;

-- | The response status code.
ldcrsResponseStatus :: Lens' ListDeploymentConfigsResponse Int
ldcrsResponseStatus = lens _ldcrsResponseStatus (\ s a -> s{_ldcrsResponseStatus = a});
