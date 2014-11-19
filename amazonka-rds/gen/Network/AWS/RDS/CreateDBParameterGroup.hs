{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB parameter group. A DB parameter group is initially created
-- with the default parameters for the database engine used by the DB
-- instance. To provide custom values for any of the parameters, you must
-- modify the group after creating it using ModifyDBParameterGroup. Once
-- you've created a DB parameter group, you need to associate it with your DB
-- instance using ModifyDBInstance. When you associate a new DB parameter
-- group with a running DB instance, you need to reboot the DB instance
-- without failover for the new DB parameter group and associated settings to
-- take effect. After you create a DB parameter group, you should wait at
-- least 5 minutes before creating your first DB instance that uses that DB
-- parameter group as the default parameter group. This allows Amazon RDS to
-- fully complete the create action before the parameter group is used as the
-- default for a new DB instance. This is especially important for parameters
-- that are critical when creating the default database for a DB instance,
-- such as the character set for the default database defined by the
-- character_set_database parameter. You can use the Parameter Groups option
-- of the Amazon RDS console or the DescribeDBParameters command to verify
-- that your DB parameter group has been created or modified.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBParameterGroup.html>
module Network.AWS.RDS.CreateDBParameterGroup
    (
    -- * Request
      CreateDBParameterGroup
    -- ** Request constructor
    , createDBParameterGroup
    -- ** Request lenses
    , cdbpg1DBParameterGroupFamily
    , cdbpg1DBParameterGroupName
    , cdbpg1Description
    , cdbpg1Tags

    -- * Response
    , CreateDBParameterGroupResponse
    -- ** Response constructor
    , createDBParameterGroupResponse
    -- ** Response lenses
    , cdbpgrDBParameterGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data CreateDBParameterGroup = CreateDBParameterGroup
    { _cdbpg1DBParameterGroupFamily :: Text
    , _cdbpg1DBParameterGroupName   :: Text
    , _cdbpg1Description            :: Text
    , _cdbpg1Tags                   :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'CreateDBParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbpg1DBParameterGroupFamily' @::@ 'Text'
--
-- * 'cdbpg1DBParameterGroupName' @::@ 'Text'
--
-- * 'cdbpg1Description' @::@ 'Text'
--
-- * 'cdbpg1Tags' @::@ ['Tag']
--
createDBParameterGroup :: Text -- ^ 'cdbpg1DBParameterGroupName'
                       -> Text -- ^ 'cdbpg1DBParameterGroupFamily'
                       -> Text -- ^ 'cdbpg1Description'
                       -> CreateDBParameterGroup
createDBParameterGroup p1 p2 p3 = CreateDBParameterGroup
    { _cdbpg1DBParameterGroupName   = p1
    , _cdbpg1DBParameterGroupFamily = p2
    , _cdbpg1Description            = p3
    , _cdbpg1Tags                   = mempty
    }

-- | The DB parameter group family name. A DB parameter group can be
-- associated with one and only one DB parameter group family, and can be
-- applied only to a DB instance running a database engine and engine
-- version compatible with that DB parameter group family.
cdbpg1DBParameterGroupFamily :: Lens' CreateDBParameterGroup Text
cdbpg1DBParameterGroupFamily =
    lens _cdbpg1DBParameterGroupFamily
        (\s a -> s { _cdbpg1DBParameterGroupFamily = a })

-- | The name of the DB parameter group. Constraints: Must be 1 to 255
-- alphanumeric characters First character must be a letter Cannot end with
-- a hyphen or contain two consecutive hyphens.
cdbpg1DBParameterGroupName :: Lens' CreateDBParameterGroup Text
cdbpg1DBParameterGroupName =
    lens _cdbpg1DBParameterGroupName
        (\s a -> s { _cdbpg1DBParameterGroupName = a })

-- | The description for the DB parameter group.
cdbpg1Description :: Lens' CreateDBParameterGroup Text
cdbpg1Description =
    lens _cdbpg1Description (\s a -> s { _cdbpg1Description = a })

cdbpg1Tags :: Lens' CreateDBParameterGroup [Tag]
cdbpg1Tags = lens _cdbpg1Tags (\s a -> s { _cdbpg1Tags = a })

newtype CreateDBParameterGroupResponse = CreateDBParameterGroupResponse
    { _cdbpgrDBParameterGroup :: Maybe DBParameterGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateDBParameterGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbpgrDBParameterGroup' @::@ 'Maybe' 'DBParameterGroup'
--
createDBParameterGroupResponse :: CreateDBParameterGroupResponse
createDBParameterGroupResponse = CreateDBParameterGroupResponse
    { _cdbpgrDBParameterGroup = Nothing
    }

cdbpgrDBParameterGroup :: Lens' CreateDBParameterGroupResponse (Maybe DBParameterGroup)
cdbpgrDBParameterGroup =
    lens _cdbpgrDBParameterGroup (\s a -> s { _cdbpgrDBParameterGroup = a })

instance ToPath CreateDBParameterGroup where
    toPath = const "/"

instance ToQuery CreateDBParameterGroup

instance ToHeaders CreateDBParameterGroup

instance AWSRequest CreateDBParameterGroup where
    type Sv CreateDBParameterGroup = RDS
    type Rs CreateDBParameterGroup = CreateDBParameterGroupResponse

    request  = post "CreateDBParameterGroup"
    response = xmlResponse

instance FromXML CreateDBParameterGroupResponse where
    parseXML = withElement "CreateDBParameterGroupResult" $ \x ->
            <$> x .@? "DBParameterGroup"
