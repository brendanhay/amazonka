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
-- Module      : Network.AWS.Redshift.CreateClusterParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Redshift parameter group.
--
--
-- Creating parameter groups is independent of creating clusters. You can associate a cluster with a parameter group when you create the cluster. You can also associate an existing cluster with a parameter group after the cluster is created by using 'ModifyCluster' .
--
-- Parameters in the parameter group define specific behavior that applies to the databases you create on the cluster. For more information about parameters and parameter groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-parameter-groups.html Amazon Redshift Parameter Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.CreateClusterParameterGroup
    (
    -- * Creating a Request
      createClusterParameterGroup
    , CreateClusterParameterGroup
    -- * Request Lenses
    , ccpgTags
    , ccpgParameterGroupName
    , ccpgParameterGroupFamily
    , ccpgDescription

    -- * Destructuring the Response
    , createClusterParameterGroupResponse
    , CreateClusterParameterGroupResponse
    -- * Response Lenses
    , ccpgrsClusterParameterGroup
    , ccpgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createClusterParameterGroup' smart constructor.
data CreateClusterParameterGroup = CreateClusterParameterGroup'
  { _ccpgTags                 :: !(Maybe [Tag])
  , _ccpgParameterGroupName   :: !Text
  , _ccpgParameterGroupFamily :: !Text
  , _ccpgDescription          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccpgTags' - A list of tag instances.
--
-- * 'ccpgParameterGroupName' - The name of the cluster parameter group. Constraints:     * Must be 1 to 255 alphanumeric characters or hyphens     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.     * Must be unique withing your AWS account.
--
-- * 'ccpgParameterGroupFamily' - The Amazon Redshift engine version to which the cluster parameter group applies. The cluster engine version determines the set of parameters. To get a list of valid parameter group family names, you can call 'DescribeClusterParameterGroups' . By default, Amazon Redshift returns a list of all the parameter groups that are owned by your AWS account, including the default parameter groups for each Amazon Redshift engine version. The parameter group family names associated with the default parameter groups provide you the valid values. For example, a valid family name is "redshift-1.0".
--
-- * 'ccpgDescription' - A description of the parameter group.
createClusterParameterGroup
    :: Text -- ^ 'ccpgParameterGroupName'
    -> Text -- ^ 'ccpgParameterGroupFamily'
    -> Text -- ^ 'ccpgDescription'
    -> CreateClusterParameterGroup
createClusterParameterGroup pParameterGroupName_ pParameterGroupFamily_ pDescription_ =
  CreateClusterParameterGroup'
    { _ccpgTags = Nothing
    , _ccpgParameterGroupName = pParameterGroupName_
    , _ccpgParameterGroupFamily = pParameterGroupFamily_
    , _ccpgDescription = pDescription_
    }


-- | A list of tag instances.
ccpgTags :: Lens' CreateClusterParameterGroup [Tag]
ccpgTags = lens _ccpgTags (\ s a -> s{_ccpgTags = a}) . _Default . _Coerce

-- | The name of the cluster parameter group. Constraints:     * Must be 1 to 255 alphanumeric characters or hyphens     * First character must be a letter.     * Cannot end with a hyphen or contain two consecutive hyphens.     * Must be unique withing your AWS account.
ccpgParameterGroupName :: Lens' CreateClusterParameterGroup Text
ccpgParameterGroupName = lens _ccpgParameterGroupName (\ s a -> s{_ccpgParameterGroupName = a})

-- | The Amazon Redshift engine version to which the cluster parameter group applies. The cluster engine version determines the set of parameters. To get a list of valid parameter group family names, you can call 'DescribeClusterParameterGroups' . By default, Amazon Redshift returns a list of all the parameter groups that are owned by your AWS account, including the default parameter groups for each Amazon Redshift engine version. The parameter group family names associated with the default parameter groups provide you the valid values. For example, a valid family name is "redshift-1.0".
ccpgParameterGroupFamily :: Lens' CreateClusterParameterGroup Text
ccpgParameterGroupFamily = lens _ccpgParameterGroupFamily (\ s a -> s{_ccpgParameterGroupFamily = a})

-- | A description of the parameter group.
ccpgDescription :: Lens' CreateClusterParameterGroup Text
ccpgDescription = lens _ccpgDescription (\ s a -> s{_ccpgDescription = a})

instance AWSRequest CreateClusterParameterGroup where
        type Rs CreateClusterParameterGroup =
             CreateClusterParameterGroupResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper
              "CreateClusterParameterGroupResult"
              (\ s h x ->
                 CreateClusterParameterGroupResponse' <$>
                   (x .@? "ClusterParameterGroup") <*>
                     (pure (fromEnum s)))

instance Hashable CreateClusterParameterGroup where

instance NFData CreateClusterParameterGroup where

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
data CreateClusterParameterGroupResponse = CreateClusterParameterGroupResponse'
  { _ccpgrsClusterParameterGroup :: !(Maybe ClusterParameterGroup)
  , _ccpgrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClusterParameterGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccpgrsClusterParameterGroup' - Undocumented member.
--
-- * 'ccpgrsResponseStatus' - -- | The response status code.
createClusterParameterGroupResponse
    :: Int -- ^ 'ccpgrsResponseStatus'
    -> CreateClusterParameterGroupResponse
createClusterParameterGroupResponse pResponseStatus_ =
  CreateClusterParameterGroupResponse'
    { _ccpgrsClusterParameterGroup = Nothing
    , _ccpgrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ccpgrsClusterParameterGroup :: Lens' CreateClusterParameterGroupResponse (Maybe ClusterParameterGroup)
ccpgrsClusterParameterGroup = lens _ccpgrsClusterParameterGroup (\ s a -> s{_ccpgrsClusterParameterGroup = a})

-- | -- | The response status code.
ccpgrsResponseStatus :: Lens' CreateClusterParameterGroupResponse Int
ccpgrsResponseStatus = lens _ccpgrsResponseStatus (\ s a -> s{_ccpgrsResponseStatus = a})

instance NFData CreateClusterParameterGroupResponse
         where
