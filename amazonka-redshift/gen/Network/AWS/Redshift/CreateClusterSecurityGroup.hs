{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateClusterSecurityGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Redshift security group. You use security groups to
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
    , crsClusterSecurityGroup
    , crsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterSecurityGroup' smart constructor.
createClusterSecurityGroup :: Text -> Text -> CreateClusterSecurityGroup
createClusterSecurityGroup pClusterSecurityGroupName_ pDescription_ =
    CreateClusterSecurityGroup'
    { _creTags = Nothing
    , _creClusterSecurityGroupName = pClusterSecurityGroupName_
    , _creDescription = pDescription_
    }

-- | A list of tag instances.
creTags :: Lens' CreateClusterSecurityGroup [Tag]
creTags = lens _creTags (\ s a -> s{_creTags = a}) . _Default . _Coerce;

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
        request = postQuery
        response
          = receiveXMLWrapper
              "CreateClusterSecurityGroupResult"
              (\ s h x ->
                 CreateClusterSecurityGroupResponse' <$>
                   (x .@? "ClusterSecurityGroup") <*>
                     (pure (fromEnum s)))

instance ToHeaders CreateClusterSecurityGroup where
        toHeaders = const mempty

instance ToPath CreateClusterSecurityGroup where
        toPath = const mempty

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
-- * 'crsClusterSecurityGroup'
--
-- * 'crsStatus'
data CreateClusterSecurityGroupResponse = CreateClusterSecurityGroupResponse'
    { _crsClusterSecurityGroup :: !(Maybe ClusterSecurityGroup)
    , _crsStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateClusterSecurityGroupResponse' smart constructor.
createClusterSecurityGroupResponse :: Int -> CreateClusterSecurityGroupResponse
createClusterSecurityGroupResponse pStatus_ =
    CreateClusterSecurityGroupResponse'
    { _crsClusterSecurityGroup = Nothing
    , _crsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
crsClusterSecurityGroup :: Lens' CreateClusterSecurityGroupResponse (Maybe ClusterSecurityGroup)
crsClusterSecurityGroup = lens _crsClusterSecurityGroup (\ s a -> s{_crsClusterSecurityGroup = a});

-- | FIXME: Undocumented member.
crsStatus :: Lens' CreateClusterSecurityGroupResponse Int
crsStatus = lens _crsStatus (\ s a -> s{_crsStatus = a});
