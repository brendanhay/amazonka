{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CodeDeploy.ListDeploymentGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Lists the deployment groups for an application registered with the
-- applicable IAM user or AWS account.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListDeploymentGroups.html>
module Network.AWS.CodeDeploy.ListDeploymentGroups
    (
    -- * Request
      ListDeploymentGroups
    -- ** Request constructor
    , listDeploymentGroups
    -- ** Request lenses
    , ldgNextToken
    , ldgApplicationName

    -- * Response
    , ListDeploymentGroupsResponse
    -- ** Response constructor
    , listDeploymentGroupsResponse
    -- ** Response lenses
    , ldgrNextToken
    , ldgrApplicationName
    , ldgrDeploymentGroups
    , ldgrStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list deployment groups operation.
--
-- /See:/ 'listDeploymentGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldgNextToken'
--
-- * 'ldgApplicationName'
data ListDeploymentGroups = ListDeploymentGroups'
    { _ldgNextToken       :: !(Maybe Text)
    , _ldgApplicationName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDeploymentGroups' smart constructor.
listDeploymentGroups :: Text -> ListDeploymentGroups
listDeploymentGroups pApplicationName =
    ListDeploymentGroups'
    { _ldgNextToken = Nothing
    , _ldgApplicationName = pApplicationName
    }

-- | An identifier that was returned from the previous list deployment groups
-- call, which can be used to return the next set of deployment groups in
-- the list.
ldgNextToken :: Lens' ListDeploymentGroups (Maybe Text)
ldgNextToken = lens _ldgNextToken (\ s a -> s{_ldgNextToken = a});

-- | The name of an existing AWS CodeDeploy application associated with the
-- applicable IAM user or AWS account.
ldgApplicationName :: Lens' ListDeploymentGroups Text
ldgApplicationName = lens _ldgApplicationName (\ s a -> s{_ldgApplicationName = a});

instance AWSRequest ListDeploymentGroups where
        type Sv ListDeploymentGroups = CodeDeploy
        type Rs ListDeploymentGroups =
             ListDeploymentGroupsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDeploymentGroupsResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "applicationName") <*>
                     (x .?> "deploymentGroups" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
              ["nextToken" .= _ldgNextToken,
               "applicationName" .= _ldgApplicationName]

instance ToPath ListDeploymentGroups where
        toPath = const "/"

instance ToQuery ListDeploymentGroups where
        toQuery = const mempty

-- | Represents the output of a list deployment groups operation.
--
-- /See:/ 'listDeploymentGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldgrNextToken'
--
-- * 'ldgrApplicationName'
--
-- * 'ldgrDeploymentGroups'
--
-- * 'ldgrStatus'
data ListDeploymentGroupsResponse = ListDeploymentGroupsResponse'
    { _ldgrNextToken        :: !(Maybe Text)
    , _ldgrApplicationName  :: !(Maybe Text)
    , _ldgrDeploymentGroups :: !(Maybe [Text])
    , _ldgrStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDeploymentGroupsResponse' smart constructor.
listDeploymentGroupsResponse :: Int -> ListDeploymentGroupsResponse
listDeploymentGroupsResponse pStatus =
    ListDeploymentGroupsResponse'
    { _ldgrNextToken = Nothing
    , _ldgrApplicationName = Nothing
    , _ldgrDeploymentGroups = Nothing
    , _ldgrStatus = pStatus
    }

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- deployment groups call to return the next set of deployment groups in
-- the list.
ldgrNextToken :: Lens' ListDeploymentGroupsResponse (Maybe Text)
ldgrNextToken = lens _ldgrNextToken (\ s a -> s{_ldgrNextToken = a});

-- | The application name.
ldgrApplicationName :: Lens' ListDeploymentGroupsResponse (Maybe Text)
ldgrApplicationName = lens _ldgrApplicationName (\ s a -> s{_ldgrApplicationName = a});

-- | A list of corresponding deployment group names.
ldgrDeploymentGroups :: Lens' ListDeploymentGroupsResponse [Text]
ldgrDeploymentGroups = lens _ldgrDeploymentGroups (\ s a -> s{_ldgrDeploymentGroups = a}) . _Default;

-- | FIXME: Undocumented member.
ldgrStatus :: Lens' ListDeploymentGroupsResponse Int
ldgrStatus = lens _ldgrStatus (\ s a -> s{_ldgrStatus = a});
