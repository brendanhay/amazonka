{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Redshift parameter group.
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
    , ccpgrsClusterParameterGroup
    , ccpgrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterParameterGroup' smart constructor.
createClusterParameterGroup :: Text -> Text -> Text -> CreateClusterParameterGroup
createClusterParameterGroup pParameterGroupName_ pParameterGroupFamily_ pDescription_ =
    CreateClusterParameterGroup'
    { _ccpgTags = Nothing
    , _ccpgParameterGroupName = pParameterGroupName_
    , _ccpgParameterGroupFamily = pParameterGroupFamily_
    , _ccpgDescription = pDescription_
    }

-- | A list of tag instances.
ccpgTags :: Lens' CreateClusterParameterGroup [Tag]
ccpgTags = lens _ccpgTags (\ s a -> s{_ccpgTags = a}) . _Default . _Coerce;

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
        request = postQuery
        response
          = receiveXMLWrapper
              "CreateClusterParameterGroupResult"
              (\ s h x ->
                 CreateClusterParameterGroupResponse' <$>
                   (x .@? "ClusterParameterGroup") <*>
                     (pure (fromEnum s)))

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
-- * 'ccpgrsClusterParameterGroup'
--
-- * 'ccpgrsStatus'
data CreateClusterParameterGroupResponse = CreateClusterParameterGroupResponse'
    { _ccpgrsClusterParameterGroup :: !(Maybe ClusterParameterGroup)
    , _ccpgrsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterParameterGroupResponse' smart constructor.
createClusterParameterGroupResponse :: Int -> CreateClusterParameterGroupResponse
createClusterParameterGroupResponse pStatus_ =
    CreateClusterParameterGroupResponse'
    { _ccpgrsClusterParameterGroup = Nothing
    , _ccpgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
ccpgrsClusterParameterGroup :: Lens' CreateClusterParameterGroupResponse (Maybe ClusterParameterGroup)
ccpgrsClusterParameterGroup = lens _ccpgrsClusterParameterGroup (\ s a -> s{_ccpgrsClusterParameterGroup = a});

-- | FIXME: Undocumented member.
ccpgrsStatus :: Lens' CreateClusterParameterGroupResponse Int
ccpgrsStatus = lens _ccpgrsStatus (\ s a -> s{_ccpgrsStatus = a});
