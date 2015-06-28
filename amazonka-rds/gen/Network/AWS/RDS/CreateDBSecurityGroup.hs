{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.CreateDBSecurityGroup
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

-- | Creates a new DB security group. DB security groups control access to a
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
    , cDBSecurityGroup
    , cStatus
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
    } deriving (Eq,Read,Show)

-- | 'CreateDBSecurityGroup' smart constructor.
createDBSecurityGroup :: Text -> Text -> CreateDBSecurityGroup
createDBSecurityGroup pDBSecurityGroupName pDBSecurityGroupDescription =
    CreateDBSecurityGroup'
    { _cdsgTags = Nothing
    , _cdsgDBSecurityGroupName = pDBSecurityGroupName
    , _cdsgDBSecurityGroupDescription = pDBSecurityGroupDescription
    }

-- | FIXME: Undocumented member.
cdsgTags :: Lens' CreateDBSecurityGroup [Tag]
cdsgTags = lens _cdsgTags (\ s a -> s{_cdsgTags = a}) . _Default;

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
                   (x .@? "DBSecurityGroup") <*> (pure s))

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
-- * 'cDBSecurityGroup'
--
-- * 'cStatus'
data CreateDBSecurityGroupResponse = CreateDBSecurityGroupResponse'
    { _cDBSecurityGroup :: !(Maybe DBSecurityGroup)
    , _cStatus          :: !Status
    } deriving (Eq,Read,Show)

-- | 'CreateDBSecurityGroupResponse' smart constructor.
createDBSecurityGroupResponse :: Status -> CreateDBSecurityGroupResponse
createDBSecurityGroupResponse pStatus =
    CreateDBSecurityGroupResponse'
    { _cDBSecurityGroup = Nothing
    , _cStatus = pStatus
    }

-- | FIXME: Undocumented member.
cDBSecurityGroup :: Lens' CreateDBSecurityGroupResponse (Maybe DBSecurityGroup)
cDBSecurityGroup = lens _cDBSecurityGroup (\ s a -> s{_cDBSecurityGroup = a});

-- | FIXME: Undocumented member.
cStatus :: Lens' CreateDBSecurityGroupResponse Status
cStatus = lens _cStatus (\ s a -> s{_cStatus = a});
