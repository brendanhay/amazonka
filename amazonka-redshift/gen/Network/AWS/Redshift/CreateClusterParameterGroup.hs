{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.CreateClusterParameterGroup
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

-- | Creates an Amazon Redshift parameter group.
--
-- Creating parameter groups is independent of creating clusters. You can
-- associate a cluster with a parameter group when you create the cluster.
-- You can also associate an existing cluster with a parameter group after
-- the cluster is created by using ModifyCluster.
--
-- Parameters in the parameter group define specific behavior that applies
-- to the databases you create on the cluster. For more information about
-- parameters and parameter groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateClusterParameterGroup.html>
module Network.AWS.Redshift.CreateClusterParameterGroup
    (
    -- * Request
      CreateClusterParameterGroup
    -- ** Request constructor
    , createClusterParameterGroup
    -- ** Request lenses
    , ccpgTags
    , ccpgParameterGroupName
    , ccpgParameterGroupFamily
    , ccpgDescription

    -- * Response
    , CreateClusterParameterGroupResponse
    -- ** Response constructor
    , createClusterParameterGroupResponse
    -- ** Response lenses
    , ccpgrClusterParameterGroup
    , ccpgrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createClusterParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgTags'
--
-- * 'ccpgParameterGroupName'
--
-- * 'ccpgParameterGroupFamily'
--
-- * 'ccpgDescription'
data CreateClusterParameterGroup = CreateClusterParameterGroup'
    { _ccpgTags                 :: !(Maybe [Tag])
    , _ccpgParameterGroupName   :: !Text
    , _ccpgParameterGroupFamily :: !Text
    , _ccpgDescription          :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateClusterParameterGroup' smart constructor.
createClusterParameterGroup :: Text -> Text -> Text -> CreateClusterParameterGroup
createClusterParameterGroup pParameterGroupName pParameterGroupFamily pDescription =
    CreateClusterParameterGroup'
    { _ccpgTags = Nothing
    , _ccpgParameterGroupName = pParameterGroupName
    , _ccpgParameterGroupFamily = pParameterGroupFamily
    , _ccpgDescription = pDescription
    }

-- | A list of tag instances.
ccpgTags :: Lens' CreateClusterParameterGroup [Tag]
ccpgTags = lens _ccpgTags (\ s a -> s{_ccpgTags = a}) . _Default;

-- | The name of the cluster parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter.
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
-- -   Must be unique withing your AWS account.
--
-- This value is stored as a lower-case string.
ccpgParameterGroupName :: Lens' CreateClusterParameterGroup Text
ccpgParameterGroupName = lens _ccpgParameterGroupName (\ s a -> s{_ccpgParameterGroupName = a});

-- | The Amazon Redshift engine version to which the cluster parameter group
-- applies. The cluster engine version determines the set of parameters.
--
-- To get a list of valid parameter group family names, you can call
-- DescribeClusterParameterGroups. By default, Amazon Redshift returns a
-- list of all the parameter groups that are owned by your AWS account,
-- including the default parameter groups for each Amazon Redshift engine
-- version. The parameter group family names associated with the default
-- parameter groups provide you the valid values. For example, a valid
-- family name is \"redshift-1.0\".
ccpgParameterGroupFamily :: Lens' CreateClusterParameterGroup Text
ccpgParameterGroupFamily = lens _ccpgParameterGroupFamily (\ s a -> s{_ccpgParameterGroupFamily = a});

-- | A description of the parameter group.
ccpgDescription :: Lens' CreateClusterParameterGroup Text
ccpgDescription = lens _ccpgDescription (\ s a -> s{_ccpgDescription = a});

instance AWSRequest CreateClusterParameterGroup where
        type Sv CreateClusterParameterGroup = Redshift
        type Rs CreateClusterParameterGroup =
             CreateClusterParameterGroupResponse
        request = post
        response
          = receiveXMLWrapper
              "CreateClusterParameterGroupResult"
              (\ s h x ->
                 CreateClusterParameterGroupResponse' <$>
                   (x .@? "ClusterParameterGroup") <*> (pure s))

instance ToHeaders CreateClusterParameterGroup where
        toHeaders = const mempty

instance ToPath CreateClusterParameterGroup where
        toPath = const "/"

instance ToQuery CreateClusterParameterGroup where
        toQuery CreateClusterParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateClusterParameterGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _ccpgTags),
               "ParameterGroupName" =: _ccpgParameterGroupName,
               "ParameterGroupFamily" =: _ccpgParameterGroupFamily,
               "Description" =: _ccpgDescription]

-- | /See:/ 'createClusterParameterGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgrClusterParameterGroup'
--
-- * 'ccpgrStatus'
data CreateClusterParameterGroupResponse = CreateClusterParameterGroupResponse'
    { _ccpgrClusterParameterGroup :: !(Maybe ClusterParameterGroup)
    , _ccpgrStatus                :: !Status
    } deriving (Eq,Show)

-- | 'CreateClusterParameterGroupResponse' smart constructor.
createClusterParameterGroupResponse :: Status -> CreateClusterParameterGroupResponse
createClusterParameterGroupResponse pStatus =
    CreateClusterParameterGroupResponse'
    { _ccpgrClusterParameterGroup = Nothing
    , _ccpgrStatus = pStatus
    }

-- | FIXME: Undocumented member.
ccpgrClusterParameterGroup :: Lens' CreateClusterParameterGroupResponse (Maybe ClusterParameterGroup)
ccpgrClusterParameterGroup = lens _ccpgrClusterParameterGroup (\ s a -> s{_ccpgrClusterParameterGroup = a});

-- | FIXME: Undocumented member.
ccpgrStatus :: Lens' CreateClusterParameterGroupResponse Status
ccpgrStatus = lens _ccpgrStatus (\ s a -> s{_ccpgrStatus = a});
