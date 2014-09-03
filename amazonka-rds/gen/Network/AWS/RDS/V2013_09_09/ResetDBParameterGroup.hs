{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.ResetDBParameterGroup
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
-- instance restart or RebootDBInstance request. https://rds.amazonaws.com/
-- ?Action=ResetDBParameterGroup &DBParameterGroupName=mydbparametergroup
-- &Parameters.member.1.ParameterName=max_user_connections
-- &Parameters.member.1.ApplyMethod=pending-reboot
-- &Parameters.member.2.ParameterName=max_allowed_packet
-- &Parameters.member.2.ApplyMethod=immediate &ResetAllParameters=false
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &AWSAccessKeyId= &Signature= mydbparametergroup
-- 071e758f-bf57-11de-9f9f-53d6aee22de9.
module Network.AWS.RDS.V2013_09_09.ResetDBParameterGroup
    (
    -- * Request
      ResetDBParameterGroup
    -- ** Request constructor
    , resetDBParameterGroup
    -- ** Request lenses
    , rdbpgmDBParameterGroupName
    , rdbpgmResetAllParameters
    , rdbpgmParameters

    -- * Response
    , ResetDBParameterGroupResponse
    -- ** Response lenses
    , dbpgnnDBParameterGroupName
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ResetDBParameterGroup' request.
resetDBParameterGroup :: Text -- ^ 'rdbpgmDBParameterGroupName'
                      -> ResetDBParameterGroup
resetDBParameterGroup p1 = ResetDBParameterGroup
    { _rdbpgmDBParameterGroupName = p1
    , _rdbpgmResetAllParameters = Nothing
    , _rdbpgmParameters = mempty
    }

data ResetDBParameterGroup = ResetDBParameterGroup
    { _rdbpgmDBParameterGroupName :: Text
      -- ^ The name of the DB parameter group. Constraints: Must be 1 to 255
      -- alphanumeric characters First character must be a letter Cannot
      -- end with a hyphen or contain two consecutive hyphens.
    , _rdbpgmResetAllParameters :: Maybe Bool
      -- ^ Specifies whether (true) or not (false) to reset all parameters
      -- in the DB parameter group to default values. Default: true.
    , _rdbpgmParameters :: [Parameter]
      -- ^ An array of parameter names, values, and the apply method for the
      -- parameter update. At least one parameter name, value, and apply
      -- method must be supplied; subsequent arguments are optional. A
      -- maximum of 20 parameters may be modified in a single request.
      -- MySQL Valid Values (for Apply method): immediate | pending-reboot
      -- You can use the immediate value with dynamic parameters only. You
      -- can use the pending-reboot value for both dynamic and static
      -- parameters, and changes are applied when DB instance reboots.
      -- Oracle Valid Values (for Apply method): pending-reboot.
    } deriving (Show, Generic)

-- | The name of the DB parameter group. Constraints: Must be 1 to 255
-- alphanumeric characters First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
rdbpgmDBParameterGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> ResetDBParameterGroup
    -> f ResetDBParameterGroup
rdbpgmDBParameterGroupName f x =
    (\y -> x { _rdbpgmDBParameterGroupName = y })
       <$> f (_rdbpgmDBParameterGroupName x)
{-# INLINE rdbpgmDBParameterGroupName #-}

-- | Specifies whether (true) or not (false) to reset all parameters in the DB
-- parameter group to default values. Default: true.
rdbpgmResetAllParameters
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> ResetDBParameterGroup
    -> f ResetDBParameterGroup
rdbpgmResetAllParameters f x =
    (\y -> x { _rdbpgmResetAllParameters = y })
       <$> f (_rdbpgmResetAllParameters x)
{-# INLINE rdbpgmResetAllParameters #-}

-- | An array of parameter names, values, and the apply method for the parameter
-- update. At least one parameter name, value, and apply method must be
-- supplied; subsequent arguments are optional. A maximum of 20 parameters may
-- be modified in a single request. MySQL Valid Values (for Apply method):
-- immediate | pending-reboot You can use the immediate value with dynamic
-- parameters only. You can use the pending-reboot value for both dynamic and
-- static parameters, and changes are applied when DB instance reboots. Oracle
-- Valid Values (for Apply method): pending-reboot.
rdbpgmParameters
    :: Functor f
    => ([Parameter]
    -> f ([Parameter]))
    -> ResetDBParameterGroup
    -> f ResetDBParameterGroup
rdbpgmParameters f x =
    (\y -> x { _rdbpgmParameters = y })
       <$> f (_rdbpgmParameters x)
{-# INLINE rdbpgmParameters #-}

instance ToQuery ResetDBParameterGroup where
    toQuery = genericQuery def

data ResetDBParameterGroupResponse = ResetDBParameterGroupResponse
    { _dbpgnnDBParameterGroupName :: Maybe Text
      -- ^ The name of the DB parameter group.
    } deriving (Show, Generic)

-- | The name of the DB parameter group.
dbpgnnDBParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ResetDBParameterGroupResponse
    -> f ResetDBParameterGroupResponse
dbpgnnDBParameterGroupName f x =
    (\y -> x { _dbpgnnDBParameterGroupName = y })
       <$> f (_dbpgnnDBParameterGroupName x)
{-# INLINE dbpgnnDBParameterGroupName #-}

instance FromXML ResetDBParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ResetDBParameterGroup where
    type Sv ResetDBParameterGroup = RDS
    type Rs ResetDBParameterGroup = ResetDBParameterGroupResponse

    request = post "ResetDBParameterGroup"
    response _ = xmlResponse
