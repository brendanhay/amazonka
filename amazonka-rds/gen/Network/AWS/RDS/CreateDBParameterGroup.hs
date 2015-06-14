{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.CreateDBParameterGroup
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

-- | Creates a new DB parameter group.
--
-- A DB parameter group is initially created with the default parameters
-- for the database engine used by the DB instance. To provide custom
-- values for any of the parameters, you must modify the group after
-- creating it using /ModifyDBParameterGroup/. Once you\'ve created a DB
-- parameter group, you need to associate it with your DB instance using
-- /ModifyDBInstance/. When you associate a new DB parameter group with a
-- running DB instance, you need to reboot the DB instance without failover
-- for the new DB parameter group and associated settings to take effect.
--
-- After you create a DB parameter group, you should wait at least 5
-- minutes before creating your first DB instance that uses that DB
-- parameter group as the default parameter group. This allows Amazon RDS
-- to fully complete the create action before the parameter group is used
-- as the default for a new DB instance. This is especially important for
-- parameters that are critical when creating the default database for a DB
-- instance, such as the character set for the default database defined by
-- the @character_set_database@ parameter. You can use the /Parameter
-- Groups/ option of the
-- <https://console.aws.amazon.com/rds/ Amazon RDS console> or the
-- /DescribeDBParameters/ command to verify that your DB parameter group
-- has been created or modified.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBParameterGroup.html>
module Network.AWS.RDS.CreateDBParameterGroup
    (
    -- * Request
      CreateDBParameterGroup
    -- ** Request constructor
    , createDBParameterGroup
    -- ** Request lenses
    , cTags
    , cDBParameterGroupName
    , cDBParameterGroupFamily
    , cDescription

    -- * Response
    , CreateDBParameterGroupResponse
    -- ** Response constructor
    , createDBParameterGroupResponse
    -- ** Response lenses
    , cdpgrDBParameterGroup
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.RDS.Types

-- | /See:/ 'createDBParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cTags'
--
-- * 'cDBParameterGroupName'
--
-- * 'cDBParameterGroupFamily'
--
-- * 'cDescription'
data CreateDBParameterGroup = CreateDBParameterGroup'{_cTags :: Maybe [Tag], _cDBParameterGroupName :: Text, _cDBParameterGroupFamily :: Text, _cDescription :: Text} deriving (Eq, Read, Show)

-- | 'CreateDBParameterGroup' smart constructor.
createDBParameterGroup :: Text -> Text -> Text -> CreateDBParameterGroup
createDBParameterGroup pDBParameterGroupName pDBParameterGroupFamily pDescription = CreateDBParameterGroup'{_cTags = Nothing, _cDBParameterGroupName = pDBParameterGroupName, _cDBParameterGroupFamily = pDBParameterGroupFamily, _cDescription = pDescription};

-- | FIXME: Undocumented member.
cTags :: Lens' CreateDBParameterGroup (Maybe [Tag])
cTags = lens _cTags (\ s a -> s{_cTags = a});

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- This value is stored as a lower-case string.
cDBParameterGroupName :: Lens' CreateDBParameterGroup Text
cDBParameterGroupName = lens _cDBParameterGroupName (\ s a -> s{_cDBParameterGroupName = a});

-- | The DB parameter group family name. A DB parameter group can be
-- associated with one and only one DB parameter group family, and can be
-- applied only to a DB instance running a database engine and engine
-- version compatible with that DB parameter group family.
cDBParameterGroupFamily :: Lens' CreateDBParameterGroup Text
cDBParameterGroupFamily = lens _cDBParameterGroupFamily (\ s a -> s{_cDBParameterGroupFamily = a});

-- | The description for the DB parameter group.
cDescription :: Lens' CreateDBParameterGroup Text
cDescription = lens _cDescription (\ s a -> s{_cDescription = a});

instance AWSRequest CreateDBParameterGroup where
        type Sv CreateDBParameterGroup = RDS
        type Rs CreateDBParameterGroup =
             CreateDBParameterGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateDBParameterGroupResult"
              (\ s h x ->
                 CreateDBParameterGroupResponse' <$>
                   x .@? "DBParameterGroup")

instance ToHeaders CreateDBParameterGroup where
        toHeaders = const mempty

instance ToPath CreateDBParameterGroup where
        toPath = const "/"

instance ToQuery CreateDBParameterGroup where
        toQuery CreateDBParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateDBParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: "Tag" =: _cTags,
               "DBParameterGroupName" =: _cDBParameterGroupName,
               "DBParameterGroupFamily" =: _cDBParameterGroupFamily,
               "Description" =: _cDescription]

-- | /See:/ 'createDBParameterGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdpgrDBParameterGroup'
newtype CreateDBParameterGroupResponse = CreateDBParameterGroupResponse'{_cdpgrDBParameterGroup :: Maybe DBParameterGroup} deriving (Eq, Read, Show)

-- | 'CreateDBParameterGroupResponse' smart constructor.
createDBParameterGroupResponse :: CreateDBParameterGroupResponse
createDBParameterGroupResponse = CreateDBParameterGroupResponse'{_cdpgrDBParameterGroup = Nothing};

-- | FIXME: Undocumented member.
cdpgrDBParameterGroup :: Lens' CreateDBParameterGroupResponse (Maybe DBParameterGroup)
cdpgrDBParameterGroup = lens _cdpgrDBParameterGroup (\ s a -> s{_cdpgrDBParameterGroup = a});
