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

-- Module      : Network.AWS.RDS.ResetDBParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the parameters of a DB parameter group to the engine/system
-- default value. To reset specific parameters submit a list of the following:
-- ParameterName and ApplyMethod. To reset the entire DB parameter group,
-- specify the DBParameterGroup name and ResetAllParameters parameters. When
-- resetting the entire group, dynamic parameters are updated immediately and
-- static parameters are set to pending-reboot to take effect on the next DB
-- instance restart or RebootDBInstance request.
module Network.AWS.RDS.ResetDBParameterGroup
    (
    -- * Request
      ResetDBParameterGroupMessage
    -- ** Request constructor
    , resetDBParameterGroup
    -- ** Request lenses
    , rdbpgmDBParameterGroupName
    , rdbpgmParameters
    , rdbpgmResetAllParameters

    -- * Response
    , DBParameterGroupNameMessage
    -- ** Response constructor
    , dbparameterGroupNameMessage
    -- ** Response lenses
    , dbpgnmDBParameterGroupName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data ResetDBParameterGroupMessage = ResetDBParameterGroupMessage
    { _rdbpgmDBParameterGroupName :: Text
    , _rdbpgmParameters           :: [Parameter]
    , _rdbpgmResetAllParameters   :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'ResetDBParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdbpgmDBParameterGroupName' @::@ 'Text'
--
-- * 'rdbpgmParameters' @::@ ['Parameter']
--
-- * 'rdbpgmResetAllParameters' @::@ 'Maybe' 'Bool'
--
resetDBParameterGroup :: Text -- ^ 'rdbpgmDBParameterGroupName'
                      -> ResetDBParameterGroupMessage
resetDBParameterGroup p1 = ResetDBParameterGroupMessage
    { _rdbpgmDBParameterGroupName = p1
    , _rdbpgmResetAllParameters   = Nothing
    , _rdbpgmParameters           = mempty
    }

-- | The name of the DB parameter group. Constraints: Must be 1 to 255
-- alphanumeric characters First character must be a letter Cannot end with
-- a hyphen or contain two consecutive hyphens.
rdbpgmDBParameterGroupName :: Lens' ResetDBParameterGroupMessage Text
rdbpgmDBParameterGroupName =
    lens _rdbpgmDBParameterGroupName
        (\s a -> s { _rdbpgmDBParameterGroupName = a })

-- | An array of parameter names, values, and the apply method for the
-- parameter update. At least one parameter name, value, and apply method
-- must be supplied; subsequent arguments are optional. A maximum of 20
-- parameters may be modified in a single request. MySQL Valid Values (for
-- Apply method): immediate | pending-reboot You can use the immediate value
-- with dynamic parameters only. You can use the pending-reboot value for
-- both dynamic and static parameters, and changes are applied when DB
-- instance reboots. Oracle Valid Values (for Apply method): pending-reboot.
rdbpgmParameters :: Lens' ResetDBParameterGroupMessage [Parameter]
rdbpgmParameters = lens _rdbpgmParameters (\s a -> s { _rdbpgmParameters = a })

-- | Specifies whether (true) or not (false) to reset all parameters in the DB
-- parameter group to default values. Default: true.
rdbpgmResetAllParameters :: Lens' ResetDBParameterGroupMessage (Maybe Bool)
rdbpgmResetAllParameters =
    lens _rdbpgmResetAllParameters
        (\s a -> s { _rdbpgmResetAllParameters = a })

instance ToQuery ResetDBParameterGroupMessage

instance ToPath ResetDBParameterGroupMessage where
    toPath = const "/"

instance AWSRequest ResetDBParameterGroupMessage where
    type Sv ResetDBParameterGroupMessage = RDS
    type Rs ResetDBParameterGroupMessage = DBParameterGroupNameMessage

    request  = post "ResetDBParameterGroup"
    response = xmlResponse $ const decodeCursor
