{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Redshift subnet group. You must provide a list of
-- one or more subnets in your existing Amazon Virtual Private Cloud
-- (Amazon VPC) when creating Amazon Redshift subnet group.
--
-- For information about subnet groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-cluster-subnet-groups.html Amazon Redshift Cluster Subnet Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateClusterSubnetGroup.html>
module Network.AWS.Redshift.CreateClusterSubnetGroup
    (
    -- * Request
      CreateClusterSubnetGroup
    -- ** Request constructor
    , createClusterSubnetGroup
    -- ** Request lenses
    , ccsgTags
    , ccsgClusterSubnetGroupName
    , ccsgDescription
    , ccsgSubnetIds

    -- * Response
    , CreateClusterSubnetGroupResponse
    -- ** Response constructor
    , createClusterSubnetGroupResponse
    -- ** Response lenses
    , ccsgrClusterSubnetGroup
    , ccsgrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createClusterSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgTags'
--
-- * 'ccsgClusterSubnetGroupName'
--
-- * 'ccsgDescription'
--
-- * 'ccsgSubnetIds'
data CreateClusterSubnetGroup = CreateClusterSubnetGroup'
    { _ccsgTags                   :: !(Maybe [Tag])
    , _ccsgClusterSubnetGroupName :: !Text
    , _ccsgDescription            :: !Text
    , _ccsgSubnetIds              :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterSubnetGroup' smart constructor.
createClusterSubnetGroup :: Text -> Text -> CreateClusterSubnetGroup
createClusterSubnetGroup pClusterSubnetGroupName pDescription =
    CreateClusterSubnetGroup'
    { _ccsgTags = Nothing
    , _ccsgClusterSubnetGroupName = pClusterSubnetGroupName
    , _ccsgDescription = pDescription
    , _ccsgSubnetIds = mempty
    }

-- | A list of tag instances.
ccsgTags :: Lens' CreateClusterSubnetGroup [Tag]
ccsgTags = lens _ccsgTags (\ s a -> s{_ccsgTags = a}) . _Default;

-- | The name for the subnet group. Amazon Redshift stores the value as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain no more than 255 alphanumeric characters or hyphens.
-- -   Must not be \"Default\".
-- -   Must be unique for all subnet groups that are created by your AWS
--     account.
--
-- Example: @examplesubnetgroup@
ccsgClusterSubnetGroupName :: Lens' CreateClusterSubnetGroup Text
ccsgClusterSubnetGroupName = lens _ccsgClusterSubnetGroupName (\ s a -> s{_ccsgClusterSubnetGroupName = a});

-- | A description for the subnet group.
ccsgDescription :: Lens' CreateClusterSubnetGroup Text
ccsgDescription = lens _ccsgDescription (\ s a -> s{_ccsgDescription = a});

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
ccsgSubnetIds :: Lens' CreateClusterSubnetGroup [Text]
ccsgSubnetIds = lens _ccsgSubnetIds (\ s a -> s{_ccsgSubnetIds = a});

instance AWSRequest CreateClusterSubnetGroup where
        type Sv CreateClusterSubnetGroup = Redshift
        type Rs CreateClusterSubnetGroup =
             CreateClusterSubnetGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateClusterSubnetGroupResult"
              (\ s h x ->
                 CreateClusterSubnetGroupResponse' <$>
                   (x .@? "ClusterSubnetGroup") <*> (pure (fromEnum s)))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrClusterSubnetGroup'
--
-- * 'ccsgrStatus'
data CreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse'
    { _ccsgrClusterSubnetGroup :: !(Maybe ClusterSubnetGroup)
    , _ccsgrStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterSubnetGroupResponse' smart constructor.
createClusterSubnetGroupResponse :: Int -> CreateClusterSubnetGroupResponse
createClusterSubnetGroupResponse pStatus =
    CreateClusterSubnetGroupResponse'
    { _ccsgrClusterSubnetGroup = Nothing
    , _ccsgrStatus = pStatus
    }

-- | FIXME: Undocumented member.
ccsgrClusterSubnetGroup :: Lens' CreateClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
ccsgrClusterSubnetGroup = lens _ccsgrClusterSubnetGroup (\ s a -> s{_ccsgrClusterSubnetGroup = a});

-- | FIXME: Undocumented member.
ccsgrStatus :: Lens' CreateClusterSubnetGroupResponse Int
ccsgrStatus = lens _ccsgrStatus (\ s a -> s{_ccsgrStatus = a});
