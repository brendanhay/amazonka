{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.ModifyDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the parameters of a DB parameter group. To modify more than one
-- parameter, submit a list of the following: ParameterName, ParameterValue,
-- and ApplyMethod. A maximum of 20 parameters can be modified in a single
-- request. After you modify a DB parameter group, you should wait at least 5
-- minutes before creating your first DB instance that uses that DB parameter
-- group as the default parameter group. This allows Amazon RDS to fully
-- complete the modify action before the parameter group is used as the
-- default for a new DB instance. This is especially important for parameters
-- that are critical when creating the default database for a DB instance,
-- such as the character set for the default database defined by the
-- character_set_database parameter. You can use the Parameter Groups option
-- of the Amazon RDS console or the DescribeDBParameters command to verify
-- that your DB parameter group has been created or modified.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyDBParameterGroup.html>
module Network.AWS.RDS.ModifyDBParameterGroup
    (
    -- * Request
      ModifyDBParameterGroup
    -- ** Request constructor
    , modifyDBParameterGroup
    -- ** Request lenses
    , mdbpgDBParameterGroupName
    , mdbpgParameters

    -- * Response
    , ModifyDBParameterGroupResponse
    -- ** Response constructor
    , modifyDBParameterGroupResponse
    -- ** Response lenses
    , mdbpgrDBParameterGroupName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data ModifyDBParameterGroup = ModifyDBParameterGroup
    { _mdbpgDBParameterGroupName :: Text
    , _mdbpgParameters           :: List "Parameter" Parameter
    } deriving (Eq, Show)

-- | 'ModifyDBParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbpgDBParameterGroupName' @::@ 'Text'
--
-- * 'mdbpgParameters' @::@ ['Parameter']
--
modifyDBParameterGroup :: Text -- ^ 'mdbpgDBParameterGroupName'
                       -> ModifyDBParameterGroup
modifyDBParameterGroup p1 = ModifyDBParameterGroup
    { _mdbpgDBParameterGroupName = p1
    , _mdbpgParameters           = mempty
    }

-- | The name of the DB parameter group. Constraints: Must be the name of an
-- existing DB parameter group Must be 1 to 255 alphanumeric characters
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens.
mdbpgDBParameterGroupName :: Lens' ModifyDBParameterGroup Text
mdbpgDBParameterGroupName =
    lens _mdbpgDBParameterGroupName
        (\s a -> s { _mdbpgDBParameterGroupName = a })

-- | An array of parameter names, values, and the apply method for the
-- parameter update. At least one parameter name, value, and apply method
-- must be supplied; subsequent arguments are optional. A maximum of 20
-- parameters may be modified in a single request. Valid Values (for the
-- application method): immediate | pending-reboot.
mdbpgParameters :: Lens' ModifyDBParameterGroup [Parameter]
mdbpgParameters = lens _mdbpgParameters (\s a -> s { _mdbpgParameters = a }) . _List

newtype ModifyDBParameterGroupResponse = ModifyDBParameterGroupResponse
    { _mdbpgrDBParameterGroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'ModifyDBParameterGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbpgrDBParameterGroupName' @::@ 'Maybe' 'Text'
--
modifyDBParameterGroupResponse :: ModifyDBParameterGroupResponse
modifyDBParameterGroupResponse = ModifyDBParameterGroupResponse
    { _mdbpgrDBParameterGroupName = Nothing
    }

-- | The name of the DB parameter group.
mdbpgrDBParameterGroupName :: Lens' ModifyDBParameterGroupResponse (Maybe Text)
mdbpgrDBParameterGroupName =
    lens _mdbpgrDBParameterGroupName
        (\s a -> s { _mdbpgrDBParameterGroupName = a })

instance ToPath ModifyDBParameterGroup where
    toPath = const "/"

instance ToQuery ModifyDBParameterGroup where
    toQuery ModifyDBParameterGroup{..} = mconcat
        [ "DBParameterGroupName" =? _mdbpgDBParameterGroupName
        , "Parameters"           =? _mdbpgParameters
        ]

instance ToHeaders ModifyDBParameterGroup

query

instance AWSRequest ModifyDBParameterGroup where
    type Sv ModifyDBParameterGroup = RDS
    type Rs ModifyDBParameterGroup = ModifyDBParameterGroupResponse

    request  = post "ModifyDBParameterGroup"
    response = xmlResponse

instance FromXML ModifyDBParameterGroupResponse where
    parseXML = withElement "ModifyDBParameterGroupResult" $ \x -> ModifyDBParameterGroupResponse
        <$> x .@? "DBParameterGroupName"
