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
    , cdsgrqTags
    , cdsgrqDBSecurityGroupName
    , cdsgrqDBSecurityGroupDescription

    -- * Response
    , CreateDBSecurityGroupResponse
    -- ** Response constructor
    , createDBSecurityGroupResponse
    -- ** Response lenses
    , cdsgrsDBSecurityGroup
    , cdsgrsStatus
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
-- * 'cdsgrqTags'
--
-- * 'cdsgrqDBSecurityGroupName'
--
-- * 'cdsgrqDBSecurityGroupDescription'
data CreateDBSecurityGroup = CreateDBSecurityGroup'
    { _cdsgrqTags                       :: !(Maybe [Tag])
    , _cdsgrqDBSecurityGroupName        :: !Text
    , _cdsgrqDBSecurityGroupDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBSecurityGroup' smart constructor.
createDBSecurityGroup :: Text -> Text -> CreateDBSecurityGroup
createDBSecurityGroup pDBSecurityGroupName pDBSecurityGroupDescription =
    CreateDBSecurityGroup'
    { _cdsgrqTags = Nothing
    , _cdsgrqDBSecurityGroupName = pDBSecurityGroupName
    , _cdsgrqDBSecurityGroupDescription = pDBSecurityGroupDescription
    }

-- | FIXME: Undocumented member.
cdsgrqTags :: Lens' CreateDBSecurityGroup [Tag]
cdsgrqTags = lens _cdsgrqTags (\ s a -> s{_cdsgrqTags = a}) . _Default;

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
cdsgrqDBSecurityGroupName :: Lens' CreateDBSecurityGroup Text
cdsgrqDBSecurityGroupName = lens _cdsgrqDBSecurityGroupName (\ s a -> s{_cdsgrqDBSecurityGroupName = a});

-- | The description for the DB security group.
cdsgrqDBSecurityGroupDescription :: Lens' CreateDBSecurityGroup Text
cdsgrqDBSecurityGroupDescription = lens _cdsgrqDBSecurityGroupDescription (\ s a -> s{_cdsgrqDBSecurityGroupDescription = a});

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
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _cdsgrqTags),
               "DBSecurityGroupName" =: _cdsgrqDBSecurityGroupName,
               "DBSecurityGroupDescription" =:
                 _cdsgrqDBSecurityGroupDescription]

-- | /See:/ 'createDBSecurityGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsgrsDBSecurityGroup'
--
-- * 'cdsgrsStatus'
data CreateDBSecurityGroupResponse = CreateDBSecurityGroupResponse'
    { _cdsgrsDBSecurityGroup :: !(Maybe DBSecurityGroup)
    , _cdsgrsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBSecurityGroupResponse' smart constructor.
createDBSecurityGroupResponse :: Int -> CreateDBSecurityGroupResponse
createDBSecurityGroupResponse pStatus =
    CreateDBSecurityGroupResponse'
    { _cdsgrsDBSecurityGroup = Nothing
    , _cdsgrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cdsgrsDBSecurityGroup :: Lens' CreateDBSecurityGroupResponse (Maybe DBSecurityGroup)
cdsgrsDBSecurityGroup = lens _cdsgrsDBSecurityGroup (\ s a -> s{_cdsgrsDBSecurityGroup = a});

-- | FIXME: Undocumented member.
cdsgrsStatus :: Lens' CreateDBSecurityGroupResponse Int
cdsgrsStatus = lens _cdsgrsStatus (\ s a -> s{_cdsgrsStatus = a});
