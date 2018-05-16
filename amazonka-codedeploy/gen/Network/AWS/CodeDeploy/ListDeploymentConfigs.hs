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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment configurations with the applicable IAM user or AWS account.
--
--
--
-- This operation returns paginated results.
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

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListDeploymentConfigs operation.
--
--
--
-- /See:/ 'listDeploymentConfigs' smart constructor.
newtype ListDeploymentConfigs = ListDeploymentConfigs'
  { _ldcNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeploymentConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldcNextToken' - An identifier returned from the previous list deployment configurations call. It can be used to return the next set of deployment configurations in the list.
listDeploymentConfigs
    :: ListDeploymentConfigs
listDeploymentConfigs = ListDeploymentConfigs' {_ldcNextToken = Nothing}


-- | An identifier returned from the previous list deployment configurations call. It can be used to return the next set of deployment configurations in the list.
ldcNextToken :: Lens' ListDeploymentConfigs (Maybe Text)
ldcNextToken = lens _ldcNextToken (\ s a -> s{_ldcNextToken = a})

instance AWSPager ListDeploymentConfigs where
        page rq rs
          | stop (rs ^. ldcrsNextToken) = Nothing
          | stop (rs ^. ldcrsDeploymentConfigsList) = Nothing
          | otherwise =
            Just $ rq & ldcNextToken .~ rs ^. ldcrsNextToken

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

instance Hashable ListDeploymentConfigs where

instance NFData ListDeploymentConfigs where

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

-- | Represents the output of a ListDeploymentConfigs operation.
--
--
--
-- /See:/ 'listDeploymentConfigsResponse' smart constructor.
data ListDeploymentConfigsResponse = ListDeploymentConfigsResponse'
  { _ldcrsNextToken             :: !(Maybe Text)
  , _ldcrsDeploymentConfigsList :: !(Maybe [Text])
  , _ldcrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeploymentConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldcrsNextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment configurations call to return the next set of deployment configurations in the list.
--
-- * 'ldcrsDeploymentConfigsList' - A list of deployment configurations, including built-in configurations such as CodeDeployDefault.OneAtATime.
--
-- * 'ldcrsResponseStatus' - -- | The response status code.
listDeploymentConfigsResponse
    :: Int -- ^ 'ldcrsResponseStatus'
    -> ListDeploymentConfigsResponse
listDeploymentConfigsResponse pResponseStatus_ =
  ListDeploymentConfigsResponse'
    { _ldcrsNextToken = Nothing
    , _ldcrsDeploymentConfigsList = Nothing
    , _ldcrsResponseStatus = pResponseStatus_
    }


-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment configurations call to return the next set of deployment configurations in the list.
ldcrsNextToken :: Lens' ListDeploymentConfigsResponse (Maybe Text)
ldcrsNextToken = lens _ldcrsNextToken (\ s a -> s{_ldcrsNextToken = a})

-- | A list of deployment configurations, including built-in configurations such as CodeDeployDefault.OneAtATime.
ldcrsDeploymentConfigsList :: Lens' ListDeploymentConfigsResponse [Text]
ldcrsDeploymentConfigsList = lens _ldcrsDeploymentConfigsList (\ s a -> s{_ldcrsDeploymentConfigsList = a}) . _Default . _Coerce

-- | -- | The response status code.
ldcrsResponseStatus :: Lens' ListDeploymentConfigsResponse Int
ldcrsResponseStatus = lens _ldcrsResponseStatus (\ s a -> s{_ldcrsResponseStatus = a})

instance NFData ListDeploymentConfigsResponse where
