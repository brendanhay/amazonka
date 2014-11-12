{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.RDS.CreateDBParameterGroup
    (
    -- * Request
      CreateDBParameterGroupMessage
    -- ** Request constructor
    , createDBParameterGroup
    -- ** Request lenses
    , cdbpgmDBParameterGroupFamily
    , cdbpgmDBParameterGroupName
    , cdbpgmDescription
    , cdbpgmTags

    -- * Response
    , CreateDBParameterGroupResult
    -- ** Response constructor
    , createDBParameterGroupResponse
    -- ** Response lenses
    , cdbpgr1DBParameterGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CreateDBParameterGroupMessage = CreateDBParameterGroupMessage
    { _cdbpgmDBParameterGroupFamily :: Text
    , _cdbpgmDBParameterGroupName   :: Text
    , _cdbpgmDescription            :: Text
    , _cdbpgmTags                   :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'CreateDBParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbpgmDBParameterGroupFamily' @::@ 'Text'
--
-- * 'cdbpgmDBParameterGroupName' @::@ 'Text'
--
-- * 'cdbpgmDescription' @::@ 'Text'
--
-- * 'cdbpgmTags' @::@ ['Tag']
--
createDBParameterGroup :: Text -- ^ 'cdbpgmDBParameterGroupName'
                       -> Text -- ^ 'cdbpgmDBParameterGroupFamily'
                       -> Text -- ^ 'cdbpgmDescription'
                       -> CreateDBParameterGroupMessage
createDBParameterGroup p1 p2 p3 = CreateDBParameterGroupMessage
    { _cdbpgmDBParameterGroupName   = p1
    , _cdbpgmDBParameterGroupFamily = p2
    , _cdbpgmDescription            = p3
    , _cdbpgmTags                   = mempty
    }

-- | The DB parameter group family name. A DB parameter group can be
-- associated with one and only one DB parameter group family, and can be
-- applied only to a DB instance running a database engine and engine
-- version compatible with that DB parameter group family.
cdbpgmDBParameterGroupFamily :: Lens' CreateDBParameterGroupMessage Text
cdbpgmDBParameterGroupFamily =
    lens _cdbpgmDBParameterGroupFamily
        (\s a -> s { _cdbpgmDBParameterGroupFamily = a })

-- | The name of the DB parameter group. Constraints: Must be 1 to 255
-- alphanumeric characters First character must be a letter Cannot end with
-- a hyphen or contain two consecutive hyphens.
cdbpgmDBParameterGroupName :: Lens' CreateDBParameterGroupMessage Text
cdbpgmDBParameterGroupName =
    lens _cdbpgmDBParameterGroupName
        (\s a -> s { _cdbpgmDBParameterGroupName = a })

-- | The description for the DB parameter group.
cdbpgmDescription :: Lens' CreateDBParameterGroupMessage Text
cdbpgmDescription =
    lens _cdbpgmDescription (\s a -> s { _cdbpgmDescription = a })

cdbpgmTags :: Lens' CreateDBParameterGroupMessage [Tag]
cdbpgmTags = lens _cdbpgmTags (\s a -> s { _cdbpgmTags = a })

instance ToQuery CreateDBParameterGroupMessage

instance ToPath CreateDBParameterGroupMessage where
    toPath = const "/"

newtype CreateDBParameterGroupResult = CreateDBParameterGroupResult
    { _cdbpgr1DBParameterGroup :: Maybe DBParameterGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateDBParameterGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbpgr1DBParameterGroup' @::@ 'Maybe' 'DBParameterGroup'
--
createDBParameterGroupResponse :: CreateDBParameterGroupResult
createDBParameterGroupResponse = CreateDBParameterGroupResult
    { _cdbpgr1DBParameterGroup = Nothing
    }

cdbpgr1DBParameterGroup :: Lens' CreateDBParameterGroupResult (Maybe DBParameterGroup)
cdbpgr1DBParameterGroup =
    lens _cdbpgr1DBParameterGroup (\s a -> s { _cdbpgr1DBParameterGroup = a })

instance FromXML CreateDBParameterGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateDBParameterGroupResult"

instance AWSRequest CreateDBParameterGroupMessage where
    type Sv CreateDBParameterGroupMessage = RDS
    type Rs CreateDBParameterGroupMessage = CreateDBParameterGroupResult

    request  = post "CreateDBParameterGroup"
    response = xmlResponse $ \h x -> CreateDBParameterGroupResult
        <$> x %| "DBParameterGroup"
