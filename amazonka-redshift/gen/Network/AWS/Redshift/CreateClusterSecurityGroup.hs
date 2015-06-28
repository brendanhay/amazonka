{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.CreateClusterSecurityGroup
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

-- | Creates a new Amazon Redshift security group. You use security groups to
-- control access to non-VPC clusters.
--
-- For information about managing security groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_CreateClusterSecurityGroup.html>
module Network.AWS.Redshift.CreateClusterSecurityGroup
    (
    -- * Request
      CreateClusterSecurityGroup
    -- ** Request constructor
    , createClusterSecurityGroup
    -- ** Request lenses
    , creTags
    , creClusterSecurityGroupName
    , creDescription

    -- * Response
    , CreateClusterSecurityGroupResponse
    -- ** Response constructor
    , createClusterSecurityGroupResponse
    -- ** Response lenses
    , creClusterSecurityGroup
    , creStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | ???
--
-- /See:/ 'createClusterSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creTags'
--
-- * 'creClusterSecurityGroupName'
--
-- * 'creDescription'
data CreateClusterSecurityGroup = CreateClusterSecurityGroup'
    { _creTags                     :: !(Maybe [Tag])
    , _creClusterSecurityGroupName :: !Text
    , _creDescription              :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateClusterSecurityGroup' smart constructor.
createClusterSecurityGroup :: Text -> Text -> CreateClusterSecurityGroup
createClusterSecurityGroup pClusterSecurityGroupName pDescription =
    CreateClusterSecurityGroup'
    { _creTags = Nothing
    , _creClusterSecurityGroupName = pClusterSecurityGroupName
    , _creDescription = pDescription
    }

-- | A list of tag instances.
creTags :: Lens' CreateClusterSecurityGroup [Tag]
creTags = lens _creTags (\ s a -> s{_creTags = a}) . _Default;

-- | The name for the security group. Amazon Redshift stores the value as a
-- lowercase string.
--
-- Constraints:
--
-- -   Must contain no more than 255 alphanumeric characters or hyphens.
-- -   Must not be \"Default\".
-- -   Must be unique for all security groups that are created by your AWS
--     account.
--
-- Example: @examplesecuritygroup@
creClusterSecurityGroupName :: Lens' CreateClusterSecurityGroup Text
creClusterSecurityGroupName = lens _creClusterSecurityGroupName (\ s a -> s{_creClusterSecurityGroupName = a});

-- | A description for the security group.
creDescription :: Lens' CreateClusterSecurityGroup Text
creDescription = lens _creDescription (\ s a -> s{_creDescription = a});

instance AWSRequest CreateClusterSecurityGroup where
        type Sv CreateClusterSecurityGroup = Redshift
        type Rs CreateClusterSecurityGroup =
             CreateClusterSecurityGroupResponse
        request = post
        response
          = receiveXMLWrapper
              "CreateClusterSecurityGroupResult"
              (\ s h x ->
                 CreateClusterSecurityGroupResponse' <$>
                   (x .@? "ClusterSecurityGroup") <*> (pure s))

instance ToHeaders CreateClusterSecurityGroup where
        toHeaders = const mempty

instance ToPath CreateClusterSecurityGroup where
        toPath = const "/"

instance ToQuery CreateClusterSecurityGroup where
        toQuery CreateClusterSecurityGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateClusterSecurityGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _creTags),
               "ClusterSecurityGroupName" =:
                 _creClusterSecurityGroupName,
               "Description" =: _creDescription]

-- | /See:/ 'createClusterSecurityGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creClusterSecurityGroup'
--
-- * 'creStatus'
data CreateClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse'
    { _creClusterSecurityGroup :: !(Maybe ClusterSecurityGroup)
    , _creStatus               :: !Status
    } deriving (Eq,Show)

-- | 'CreateClusterSecurityGroupResponse' smart constructor.
createClusterSecurityGroupResponse :: Status -> CreateClusterSecurityGroupResponse
createClusterSecurityGroupResponse pStatus =
    CreateClusterSecurityGroupResponse'
    { _creClusterSecurityGroup = Nothing
    , _creStatus = pStatus
    }

-- | FIXME: Undocumented member.
creClusterSecurityGroup :: Lens' CreateClusterSecurityGroupResponse (Maybe ClusterSecurityGroup)
creClusterSecurityGroup = lens _creClusterSecurityGroup (\ s a -> s{_creClusterSecurityGroup = a});

-- | FIXME: Undocumented member.
creStatus :: Lens' CreateClusterSecurityGroupResponse Status
creStatus = lens _creStatus (\ s a -> s{_creStatus = a});
