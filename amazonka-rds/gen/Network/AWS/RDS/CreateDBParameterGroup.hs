{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB parameter group.
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
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBParameterGroup.html AWS API Reference> for CreateDBParameterGroup.
module Network.AWS.RDS.CreateDBParameterGroup
    (
    -- * Creating a Request
      CreateDBParameterGroup
    , createDBParameterGroup
    -- * Request Lenses
    , cdbpgTags
    , cdbpgDBParameterGroupName
    , cdbpgDBParameterGroupFamily
    , cdbpgDescription

    -- * Destructuring the Response
    , CreateDBParameterGroupResponse
    , createDBParameterGroupResponse
    -- * Response Lenses
    , cdpgrsDBParameterGroup
    , cdpgrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createDBParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbpgTags'
--
-- * 'cdbpgDBParameterGroupName'
--
-- * 'cdbpgDBParameterGroupFamily'
--
-- * 'cdbpgDescription'
data CreateDBParameterGroup = CreateDBParameterGroup'
    { _cdbpgTags                   :: !(Maybe [Tag])
    , _cdbpgDBParameterGroupName   :: !Text
    , _cdbpgDBParameterGroupFamily :: !Text
    , _cdbpgDescription            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBParameterGroup' smart constructor.
createDBParameterGroup :: Text -> Text -> Text -> CreateDBParameterGroup
createDBParameterGroup pDBParameterGroupName_ pDBParameterGroupFamily_ pDescription_ =
    CreateDBParameterGroup'
    { _cdbpgTags = Nothing
    , _cdbpgDBParameterGroupName = pDBParameterGroupName_
    , _cdbpgDBParameterGroupFamily = pDBParameterGroupFamily_
    , _cdbpgDescription = pDescription_
    }

-- | Undocumented member.
cdbpgTags :: Lens' CreateDBParameterGroup [Tag]
cdbpgTags = lens _cdbpgTags (\ s a -> s{_cdbpgTags = a}) . _Default . _Coerce;

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- This value is stored as a lowercase string.
cdbpgDBParameterGroupName :: Lens' CreateDBParameterGroup Text
cdbpgDBParameterGroupName = lens _cdbpgDBParameterGroupName (\ s a -> s{_cdbpgDBParameterGroupName = a});

-- | The DB parameter group family name. A DB parameter group can be
-- associated with one and only one DB parameter group family, and can be
-- applied only to a DB instance running a database engine and engine
-- version compatible with that DB parameter group family.
cdbpgDBParameterGroupFamily :: Lens' CreateDBParameterGroup Text
cdbpgDBParameterGroupFamily = lens _cdbpgDBParameterGroupFamily (\ s a -> s{_cdbpgDBParameterGroupFamily = a});

-- | The description for the DB parameter group.
cdbpgDescription :: Lens' CreateDBParameterGroup Text
cdbpgDescription = lens _cdbpgDescription (\ s a -> s{_cdbpgDescription = a});

instance AWSRequest CreateDBParameterGroup where
        type Sv CreateDBParameterGroup = RDS
        type Rs CreateDBParameterGroup =
             CreateDBParameterGroupResponse
        request = postQuery
        response
          = receiveXMLWrapper "CreateDBParameterGroupResult"
              (\ s h x ->
                 CreateDBParameterGroupResponse' <$>
                   (x .@? "DBParameterGroup") <*> (pure (fromEnum s)))

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
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdbpgTags),
               "DBParameterGroupName" =: _cdbpgDBParameterGroupName,
               "DBParameterGroupFamily" =:
                 _cdbpgDBParameterGroupFamily,
               "Description" =: _cdbpgDescription]

-- | /See:/ 'createDBParameterGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdpgrsDBParameterGroup'
--
-- * 'cdpgrsStatus'
data CreateDBParameterGroupResponse = CreateDBParameterGroupResponse'
    { _cdpgrsDBParameterGroup :: !(Maybe DBParameterGroup)
    , _cdpgrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBParameterGroupResponse' smart constructor.
createDBParameterGroupResponse :: Int -> CreateDBParameterGroupResponse
createDBParameterGroupResponse pStatus_ =
    CreateDBParameterGroupResponse'
    { _cdpgrsDBParameterGroup = Nothing
    , _cdpgrsStatus = pStatus_
    }

-- | Undocumented member.
cdpgrsDBParameterGroup :: Lens' CreateDBParameterGroupResponse (Maybe DBParameterGroup)
cdpgrsDBParameterGroup = lens _cdpgrsDBParameterGroup (\ s a -> s{_cdpgrsDBParameterGroup = a});

-- | Undocumented member.
cdpgrsStatus :: Lens' CreateDBParameterGroupResponse Int
cdpgrsStatus = lens _cdpgrsStatus (\ s a -> s{_cdpgrsStatus = a});
