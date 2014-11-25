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

-- Module      : Network.AWS.RDS.ResetDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the parameters of a DB parameter group to the engine/system default
-- value. To reset specific parameters submit a list of the following: 'ParameterName' and 'ApplyMethod'. To reset the entire DB parameter group, specify the 'DBParameterGroup' name and 'ResetAllParameters' parameters. When resetting the entire group,
-- dynamic parameters are updated immediately and static parameters are set to 'pending-reboot' to take effect on the next DB instance restart or 'RebootDBInstance' request.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ResetDBParameterGroup.html>
module Network.AWS.RDS.ResetDBParameterGroup
    (
    -- * Request
      ResetDBParameterGroup
    -- ** Request constructor
    , resetDBParameterGroup
    -- ** Request lenses
    , rdbpgDBParameterGroupName
    , rdbpgParameters
    , rdbpgResetAllParameters

    -- * Response
    , ResetDBParameterGroupResponse
    -- ** Response constructor
    , resetDBParameterGroupResponse
    -- ** Response lenses
    , rdbpgrDBParameterGroupName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data ResetDBParameterGroup = ResetDBParameterGroup
    { _rdbpgDBParameterGroupName :: Text
    , _rdbpgParameters           :: List "Parameter" Parameter
    , _rdbpgResetAllParameters   :: Maybe Bool
    } deriving (Eq, Show)

-- | 'ResetDBParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbpgDBParameterGroupName' @::@ 'Text'
--
-- * 'rdbpgParameters' @::@ ['Parameter']
--
-- * 'rdbpgResetAllParameters' @::@ 'Maybe' 'Bool'
--
resetDBParameterGroup :: Text -- ^ 'rdbpgDBParameterGroupName'
                      -> ResetDBParameterGroup
resetDBParameterGroup p1 = ResetDBParameterGroup
    { _rdbpgDBParameterGroupName = p1
    , _rdbpgResetAllParameters   = Nothing
    , _rdbpgParameters           = mempty
    }

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- Must be 1 to 255 alphanumeric characters First character must be a letter Cannot end with a hyphen or contain two consecutive hyphens
--
rdbpgDBParameterGroupName :: Lens' ResetDBParameterGroup Text
rdbpgDBParameterGroupName =
    lens _rdbpgDBParameterGroupName
        (\s a -> s { _rdbpgDBParameterGroupName = a })

-- | An array of parameter names, values, and the apply method for the parameter
-- update. At least one parameter name, value, and apply method must be
-- supplied; subsequent arguments are optional. A maximum of 20 parameters may
-- be modified in a single request.
--
-- MySQL
--
-- Valid Values (for Apply method): 'immediate' | 'pending-reboot'
--
-- You can use the immediate value with dynamic parameters only. You can use
-- the 'pending-reboot' value for both dynamic and static parameters, and changes
-- are applied when DB instance reboots.
--
-- Oracle
--
-- Valid Values (for Apply method): 'pending-reboot'
--
rdbpgParameters :: Lens' ResetDBParameterGroup [Parameter]
rdbpgParameters = lens _rdbpgParameters (\s a -> s { _rdbpgParameters = a }) . _List

-- | Specifies whether ('true') or not ('false') to reset all parameters in the DB
-- parameter group to default values.
--
-- Default: 'true'
--
rdbpgResetAllParameters :: Lens' ResetDBParameterGroup (Maybe Bool)
rdbpgResetAllParameters =
    lens _rdbpgResetAllParameters (\s a -> s { _rdbpgResetAllParameters = a })

newtype ResetDBParameterGroupResponse = ResetDBParameterGroupResponse
    { _rdbpgrDBParameterGroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'ResetDBParameterGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbpgrDBParameterGroupName' @::@ 'Maybe' 'Text'
--
resetDBParameterGroupResponse :: ResetDBParameterGroupResponse
resetDBParameterGroupResponse = ResetDBParameterGroupResponse
    { _rdbpgrDBParameterGroupName = Nothing
    }

-- | The name of the DB parameter group.
--
rdbpgrDBParameterGroupName :: Lens' ResetDBParameterGroupResponse (Maybe Text)
rdbpgrDBParameterGroupName =
    lens _rdbpgrDBParameterGroupName
        (\s a -> s { _rdbpgrDBParameterGroupName = a })

instance ToPath ResetDBParameterGroup where
    toPath = const "/"

instance ToQuery ResetDBParameterGroup where
    toQuery ResetDBParameterGroup{..} = mconcat
        [ "DBParameterGroupName" =? _rdbpgDBParameterGroupName
        , "Parameters"           =? _rdbpgParameters
        , "ResetAllParameters"   =? _rdbpgResetAllParameters
        ]

instance ToHeaders ResetDBParameterGroup

instance AWSRequest ResetDBParameterGroup where
    type Sv ResetDBParameterGroup = RDS
    type Rs ResetDBParameterGroup = ResetDBParameterGroupResponse

    request  = post "ResetDBParameterGroup"
    response = xmlResponse

instance FromXML ResetDBParameterGroupResponse where
    parseXML = withElement "ResetDBParameterGroupResult" $ \x -> ResetDBParameterGroupResponse
        <$> x .@? "DBParameterGroupName"
