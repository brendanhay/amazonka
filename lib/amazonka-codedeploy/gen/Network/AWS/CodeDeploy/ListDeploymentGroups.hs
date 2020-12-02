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
-- Module      : Network.AWS.CodeDeploy.ListDeploymentGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the deployment groups for an application registered with the applicable IAM user or AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListDeploymentGroups
    (
    -- * Creating a Request
      listDeploymentGroups
    , ListDeploymentGroups
    -- * Request Lenses
    , ldgNextToken
    , ldgApplicationName

    -- * Destructuring the Response
    , listDeploymentGroupsResponse
    , ListDeploymentGroupsResponse
    -- * Response Lenses
    , ldgrsNextToken
    , ldgrsApplicationName
    , ldgrsDeploymentGroups
    , ldgrsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListDeploymentGroups operation.
--
--
--
-- /See:/ 'listDeploymentGroups' smart constructor.
data ListDeploymentGroups = ListDeploymentGroups'
  { _ldgNextToken       :: !(Maybe Text)
  , _ldgApplicationName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeploymentGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldgNextToken' - An identifier returned from the previous list deployment groups call. It can be used to return the next set of deployment groups in the list.
--
-- * 'ldgApplicationName' - The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
listDeploymentGroups
    :: Text -- ^ 'ldgApplicationName'
    -> ListDeploymentGroups
listDeploymentGroups pApplicationName_ =
  ListDeploymentGroups'
    {_ldgNextToken = Nothing, _ldgApplicationName = pApplicationName_}


-- | An identifier returned from the previous list deployment groups call. It can be used to return the next set of deployment groups in the list.
ldgNextToken :: Lens' ListDeploymentGroups (Maybe Text)
ldgNextToken = lens _ldgNextToken (\ s a -> s{_ldgNextToken = a})

-- | The name of an AWS CodeDeploy application associated with the applicable IAM user or AWS account.
ldgApplicationName :: Lens' ListDeploymentGroups Text
ldgApplicationName = lens _ldgApplicationName (\ s a -> s{_ldgApplicationName = a})

instance AWSPager ListDeploymentGroups where
        page rq rs
          | stop (rs ^. ldgrsNextToken) = Nothing
          | stop (rs ^. ldgrsDeploymentGroups) = Nothing
          | otherwise =
            Just $ rq & ldgNextToken .~ rs ^. ldgrsNextToken

instance AWSRequest ListDeploymentGroups where
        type Rs ListDeploymentGroups =
             ListDeploymentGroupsResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 ListDeploymentGroupsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "applicationName") <*>
                     (x .?> "deploymentGroups" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListDeploymentGroups where

instance NFData ListDeploymentGroups where

instance ToHeaders ListDeploymentGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ListDeploymentGroups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDeploymentGroups where
        toJSON ListDeploymentGroups'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _ldgNextToken,
                  Just ("applicationName" .= _ldgApplicationName)])

instance ToPath ListDeploymentGroups where
        toPath = const "/"

instance ToQuery ListDeploymentGroups where
        toQuery = const mempty

-- | Represents the output of a ListDeploymentGroups operation.
--
--
--
-- /See:/ 'listDeploymentGroupsResponse' smart constructor.
data ListDeploymentGroupsResponse = ListDeploymentGroupsResponse'
  { _ldgrsNextToken        :: !(Maybe Text)
  , _ldgrsApplicationName  :: !(Maybe Text)
  , _ldgrsDeploymentGroups :: !(Maybe [Text])
  , _ldgrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDeploymentGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldgrsNextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment groups call to return the next set of deployment groups in the list.
--
-- * 'ldgrsApplicationName' - The application name.
--
-- * 'ldgrsDeploymentGroups' - A list of corresponding deployment group names.
--
-- * 'ldgrsResponseStatus' - -- | The response status code.
listDeploymentGroupsResponse
    :: Int -- ^ 'ldgrsResponseStatus'
    -> ListDeploymentGroupsResponse
listDeploymentGroupsResponse pResponseStatus_ =
  ListDeploymentGroupsResponse'
    { _ldgrsNextToken = Nothing
    , _ldgrsApplicationName = Nothing
    , _ldgrsDeploymentGroups = Nothing
    , _ldgrsResponseStatus = pResponseStatus_
    }


-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list deployment groups call to return the next set of deployment groups in the list.
ldgrsNextToken :: Lens' ListDeploymentGroupsResponse (Maybe Text)
ldgrsNextToken = lens _ldgrsNextToken (\ s a -> s{_ldgrsNextToken = a})

-- | The application name.
ldgrsApplicationName :: Lens' ListDeploymentGroupsResponse (Maybe Text)
ldgrsApplicationName = lens _ldgrsApplicationName (\ s a -> s{_ldgrsApplicationName = a})

-- | A list of corresponding deployment group names.
ldgrsDeploymentGroups :: Lens' ListDeploymentGroupsResponse [Text]
ldgrsDeploymentGroups = lens _ldgrsDeploymentGroups (\ s a -> s{_ldgrsDeploymentGroups = a}) . _Default . _Coerce

-- | -- | The response status code.
ldgrsResponseStatus :: Lens' ListDeploymentGroupsResponse Int
ldgrsResponseStatus = lens _ldgrsResponseStatus (\ s a -> s{_ldgrsResponseStatus = a})

instance NFData ListDeploymentGroupsResponse where
