{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSecurityGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB security group. DB security groups control access to a
-- DB instance.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBSecurityGroup.html>
module Network.AWS.RDS.CreateDBSecurityGroup
    (
    -- * Request
      CreateDBSecurityGroup
    -- ** Request constructor
    , createDBSecurityGroup
    -- ** Request lenses
    , cdsgTags
    , cdsgDBSecurityGroupName
    , cdsgDBSecurityGroupDescription

    -- * Response
    , CreateDBSecurityGroupResponse
    -- ** Response constructor
    , createDBSecurityGroupResponse
    -- ** Response lenses
    , cdbsgrsDBSecurityGroup
    , cdbsgrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createDBSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsgTags'
--
-- * 'cdsgDBSecurityGroupName'
--
-- * 'cdsgDBSecurityGroupDescription'
data CreateDBSecurityGroup = CreateDBSecurityGroup'
    { _cdsgTags                       :: !(Maybe [Tag])
    , _cdsgDBSecurityGroupName        :: !Text
    , _cdsgDBSecurityGroupDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBSecurityGroup' smart constructor.
createDBSecurityGroup :: Text -> Text -> CreateDBSecurityGroup
createDBSecurityGroup pDBSecurityGroupName_ pDBSecurityGroupDescription_ =
    CreateDBSecurityGroup'
    { _cdsgTags = Nothing
    , _cdsgDBSecurityGroupName = pDBSecurityGroupName_
    , _cdsgDBSecurityGroupDescription = pDBSecurityGroupDescription_
    }

-- | FIXME: Undocumented member.
cdsgTags :: Lens' CreateDBSecurityGroup [Tag]
cdsgTags = lens _cdsgTags (\ s a -> s{_cdsgTags = a}) . _Default . _Coerce;

-- | The name for the DB security group. This value is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
-- -   Must not be \"Default\"
-- -   May not contain spaces
--
-- Example: @mysecuritygroup@
cdsgDBSecurityGroupName :: Lens' CreateDBSecurityGroup Text
cdsgDBSecurityGroupName = lens _cdsgDBSecurityGroupName (\ s a -> s{_cdsgDBSecurityGroupName = a});

-- | The description for the DB security group.
cdsgDBSecurityGroupDescription :: Lens' CreateDBSecurityGroup Text
cdsgDBSecurityGroupDescription = lens _cdsgDBSecurityGroupDescription (\ s a -> s{_cdsgDBSecurityGroupDescription = a});

instance AWSRequest CreateDBSecurityGroup where
        type Sv CreateDBSecurityGroup = RDS
        type Rs CreateDBSecurityGroup =
             CreateDBSecurityGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateDBSecurityGroupResult"
              (\ s h x ->
                 CreateDBSecurityGroupResponse' <$>
                   (x .@? "DBSecurityGroup") <*> (pure (fromEnum s)))

instance ToHeaders CreateDBSecurityGroup where
        toHeaders = const mempty

instance ToPath CreateDBSecurityGroup where
        toPath = const "/"

instance ToQuery CreateDBSecurityGroup where
        toQuery CreateDBSecurityGroup'{..}
          = mconcat
              ["Action" =: ("CreateDBSecurityGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdsgTags),
               "DBSecurityGroupName" =: _cdsgDBSecurityGroupName,
               "DBSecurityGroupDescription" =:
                 _cdsgDBSecurityGroupDescription]

-- | /See:/ 'createDBSecurityGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsgrsDBSecurityGroup'
--
-- * 'cdbsgrsStatus'
data CreateDBSecurityGroupResponse = CreateDBSecurityGroupResponse'
    { _cdbsgrsDBSecurityGroup :: !(Maybe DBSecurityGroup)
    , _cdbsgrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBSecurityGroupResponse' smart constructor.
createDBSecurityGroupResponse :: Int -> CreateDBSecurityGroupResponse
createDBSecurityGroupResponse pStatus_ =
    CreateDBSecurityGroupResponse'
    { _cdbsgrsDBSecurityGroup = Nothing
    , _cdbsgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
cdbsgrsDBSecurityGroup :: Lens' CreateDBSecurityGroupResponse (Maybe DBSecurityGroup)
cdbsgrsDBSecurityGroup = lens _cdbsgrsDBSecurityGroup (\ s a -> s{_cdbsgrsDBSecurityGroup = a});

-- | FIXME: Undocumented member.
cdbsgrsStatus :: Lens' CreateDBSecurityGroupResponse Int
cdbsgrsStatus = lens _cdbsgrsStatus (\ s a -> s{_cdbsgrsStatus = a});
