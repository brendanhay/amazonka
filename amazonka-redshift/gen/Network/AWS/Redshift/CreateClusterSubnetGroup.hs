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
    , ccsgrqTags
    , ccsgrqClusterSubnetGroupName
    , ccsgrqDescription
    , ccsgrqSubnetIds

    -- * Response
    , CreateClusterSubnetGroupResponse
    -- ** Response constructor
    , createClusterSubnetGroupResponse
    -- ** Response lenses
    , ccsgrsClusterSubnetGroup
    , ccsgrsStatus
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
-- * 'ccsgrqTags'
--
-- * 'ccsgrqClusterSubnetGroupName'
--
-- * 'ccsgrqDescription'
--
-- * 'ccsgrqSubnetIds'
data CreateClusterSubnetGroup = CreateClusterSubnetGroup'
    { _ccsgrqTags                   :: !(Maybe [Tag])
    , _ccsgrqClusterSubnetGroupName :: !Text
    , _ccsgrqDescription            :: !Text
    , _ccsgrqSubnetIds              :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterSubnetGroup' smart constructor.
createClusterSubnetGroup :: Text -> Text -> CreateClusterSubnetGroup
createClusterSubnetGroup pClusterSubnetGroupName pDescription =
    CreateClusterSubnetGroup'
    { _ccsgrqTags = Nothing
    , _ccsgrqClusterSubnetGroupName = pClusterSubnetGroupName
    , _ccsgrqDescription = pDescription
    , _ccsgrqSubnetIds = mempty
    }

-- | A list of tag instances.
ccsgrqTags :: Lens' CreateClusterSubnetGroup [Tag]
ccsgrqTags = lens _ccsgrqTags (\ s a -> s{_ccsgrqTags = a}) . _Default;

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
ccsgrqClusterSubnetGroupName :: Lens' CreateClusterSubnetGroup Text
ccsgrqClusterSubnetGroupName = lens _ccsgrqClusterSubnetGroupName (\ s a -> s{_ccsgrqClusterSubnetGroupName = a});

-- | A description for the subnet group.
ccsgrqDescription :: Lens' CreateClusterSubnetGroup Text
ccsgrqDescription = lens _ccsgrqDescription (\ s a -> s{_ccsgrqDescription = a});

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
ccsgrqSubnetIds :: Lens' CreateClusterSubnetGroup [Text]
ccsgrqSubnetIds = lens _ccsgrqSubnetIds (\ s a -> s{_ccsgrqSubnetIds = a});

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
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _ccsgrqTags),
               "ClusterSubnetGroupName" =:
                 _ccsgrqClusterSubnetGroupName,
               "Description" =: _ccsgrqDescription,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _ccsgrqSubnetIds]

-- | /See:/ 'createClusterSubnetGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrsClusterSubnetGroup'
--
-- * 'ccsgrsStatus'
data CreateClusterSubnetGroupResponse = CreateClusterSubnetGroupResponse'
    { _ccsgrsClusterSubnetGroup :: !(Maybe ClusterSubnetGroup)
    , _ccsgrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterSubnetGroupResponse' smart constructor.
createClusterSubnetGroupResponse :: Int -> CreateClusterSubnetGroupResponse
createClusterSubnetGroupResponse pStatus =
    CreateClusterSubnetGroupResponse'
    { _ccsgrsClusterSubnetGroup = Nothing
    , _ccsgrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
ccsgrsClusterSubnetGroup :: Lens' CreateClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
ccsgrsClusterSubnetGroup = lens _ccsgrsClusterSubnetGroup (\ s a -> s{_ccsgrsClusterSubnetGroup = a});

-- | FIXME: Undocumented member.
ccsgrsStatus :: Lens' CreateClusterSubnetGroupResponse Int
ccsgrsStatus = lens _ccsgrsStatus (\ s a -> s{_ccsgrsStatus = a});
