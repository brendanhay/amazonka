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
-- Module      : Network.AWS.Redshift.CreateClusterSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Redshift subnet group. You must provide a list of one or more subnets in your existing Amazon Virtual Private Cloud (Amazon VPC) when creating Amazon Redshift subnet group.
--
--
-- For information about subnet groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-cluster-subnet-groups.html Amazon Redshift Cluster Subnet Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.CreateClusterSubnetGroup
    (
    -- * Creating a Request
      createClusterSubnetGroup
    , CreateClusterSubnetGroup
    -- * Request Lenses
    , ccsgTags
    , ccsgClusterSubnetGroupName
    , ccsgDescription
    , ccsgSubnetIds

    -- * Destructuring the Response
    , createClusterSubnetGroupResponse
    , CreateClusterSubnetGroupResponse
    -- * Response Lenses
    , ccsgrsClusterSubnetGroup
    , ccsgrsResponseStatus
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
-- /See:/ 'createClusterSubnetGroup' smart constructor.
data CreateClusterSubnetGroup = CreateClusterSubnetGroup'
  { _ccsgTags                   :: !(Maybe [Tag])
  , _ccsgClusterSubnetGroupName :: !Text
  , _ccsgDescription            :: !Text
  , _ccsgSubnetIds              :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClusterSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsgTags' - A list of tag instances.
--
-- * 'ccsgClusterSubnetGroupName' - The name for the subnet group. Amazon Redshift stores the value as a lowercase string. Constraints:     * Must contain no more than 255 alphanumeric characters or hyphens.     * Must not be "Default".     * Must be unique for all subnet groups that are created by your AWS account. Example: @examplesubnetgroup@
--
-- * 'ccsgDescription' - A description for the subnet group.
--
-- * 'ccsgSubnetIds' - An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
createClusterSubnetGroup
    :: Text -- ^ 'ccsgClusterSubnetGroupName'
    -> Text -- ^ 'ccsgDescription'
    -> CreateClusterSubnetGroup
createClusterSubnetGroup pClusterSubnetGroupName_ pDescription_ =
  CreateClusterSubnetGroup'
    { _ccsgTags = Nothing
    , _ccsgClusterSubnetGroupName = pClusterSubnetGroupName_
    , _ccsgDescription = pDescription_
    , _ccsgSubnetIds = mempty
    }


-- | A list of tag instances.
ccsgTags :: Lens' CreateClusterSubnetGroup [Tag]
ccsgTags = lens _ccsgTags (\ s a -> s{_ccsgTags = a}) . _Default . _Coerce

-- | The name for the subnet group. Amazon Redshift stores the value as a lowercase string. Constraints:     * Must contain no more than 255 alphanumeric characters or hyphens.     * Must not be "Default".     * Must be unique for all subnet groups that are created by your AWS account. Example: @examplesubnetgroup@
ccsgClusterSubnetGroupName :: Lens' CreateClusterSubnetGroup Text
ccsgClusterSubnetGroupName = lens _ccsgClusterSubnetGroupName (\ s a -> s{_ccsgClusterSubnetGroupName = a})

-- | A description for the subnet group.
ccsgDescription :: Lens' CreateClusterSubnetGroup Text
ccsgDescription = lens _ccsgDescription (\ s a -> s{_ccsgDescription = a})

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a single request.
ccsgSubnetIds :: Lens' CreateClusterSubnetGroup [Text]
ccsgSubnetIds = lens _ccsgSubnetIds (\ s a -> s{_ccsgSubnetIds = a}) . _Coerce

instance AWSRequest CreateClusterSubnetGroup where
        type Rs CreateClusterSubnetGroup =
             CreateClusterSubnetGroupResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "CreateClusterSubnetGroupResult"
              (\ s h x ->
                 CreateClusterSubnetGroupResponse' <$>
                   (x .@? "ClusterSubnetGroup") <*> (pure (fromEnum s)))

instance Hashable CreateClusterSubnetGroup where

instance NFData CreateClusterSubnetGroup where

instance ToHeaders CreateClusterSubnetGroup where
        toHeaders = const mempty

instance ToPath CreateClusterSubnetGroup where
        toPath = const "/"

instance ToQuery CreateClusterSubnetGroup where
        toQuery CreateClusterSubnetGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateClusterSubnetGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _ccsgTags),
               "ClusterSubnetGroupName" =:
                 _ccsgClusterSubnetGroupName,
               "Description" =: _ccsgDescription,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _ccsgSubnetIds]

-- | /See:/ 'createClusterSubnetGroupResponse' smart constructor.
data CreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse'
  { _ccsgrsClusterSubnetGroup :: !(Maybe ClusterSubnetGroup)
  , _ccsgrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClusterSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsgrsClusterSubnetGroup' - Undocumented member.
--
-- * 'ccsgrsResponseStatus' - -- | The response status code.
createClusterSubnetGroupResponse
    :: Int -- ^ 'ccsgrsResponseStatus'
    -> CreateClusterSubnetGroupResponse
createClusterSubnetGroupResponse pResponseStatus_ =
  CreateClusterSubnetGroupResponse'
    { _ccsgrsClusterSubnetGroup = Nothing
    , _ccsgrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ccsgrsClusterSubnetGroup :: Lens' CreateClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
ccsgrsClusterSubnetGroup = lens _ccsgrsClusterSubnetGroup (\ s a -> s{_ccsgrsClusterSubnetGroup = a})

-- | -- | The response status code.
ccsgrsResponseStatus :: Lens' CreateClusterSubnetGroupResponse Int
ccsgrsResponseStatus = lens _ccsgrsResponseStatus (\ s a -> s{_ccsgrsResponseStatus = a})

instance NFData CreateClusterSubnetGroupResponse
         where
