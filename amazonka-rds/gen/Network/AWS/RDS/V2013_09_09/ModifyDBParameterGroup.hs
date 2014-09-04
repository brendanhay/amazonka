{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.ModifyDBParameterGroup
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
-- request. The apply-immediate method can be used only for dynamic
-- parameters; the pending-reboot method can be used with MySQL and Oracle DB
-- instances for either dynamic or static parameters. For Microsoft SQL Server
-- DB instances, the pending-reboot method can be used only for static
-- parameters. https://rds.amazonaws.com/ ?Action=ModifyDBParameterGroup
-- &DBParameterGroupName=mydbparametergroup
-- &Parameters.member.1.ParameterName=max_user_connections
-- &Parameters.member.1.ParameterValue=24
-- &Parameters.member.1.ApplyMethod=pending-reboot
-- &Parameters.member.2.ParameterName=max_allowed_packet
-- &Parameters.member.2.ParameterValue=1024
-- &Parameters.member.2.ApplyMethod=immediate &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-11T21%3A25%3A00.686Z &AWSAccessKeyId= &Signature=
-- mydbparametergroup 5ba91f97-bf51-11de-bf60-ef2e377db6f3.
module Network.AWS.RDS.V2013_09_09.ModifyDBParameterGroup
    (
    -- * Request
      ModifyDBParameterGroup
    -- ** Request constructor
    , mkModifyDBParameterGroupMessage
    -- ** Request lenses
    , mdbpgmDBParameterGroupName
    , mdbpgmParameters

    -- * Response
    , ModifyDBParameterGroupResponse
    -- ** Response lenses
    , dbpgnmDBParameterGroupName
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyDBParameterGroup' request.
mkModifyDBParameterGroupMessage :: Text -- ^ 'mdbpgmDBParameterGroupName'
                                -> [Parameter] -- ^ 'mdbpgmParameters'
                                -> ModifyDBParameterGroup
mkModifyDBParameterGroupMessage p1 p2 = ModifyDBParameterGroup
    { _mdbpgmDBParameterGroupName = p1
    , _mdbpgmParameters = p2
    }
{-# INLINE mkModifyDBParameterGroupMessage #-}

data ModifyDBParameterGroup = ModifyDBParameterGroup
    { _mdbpgmDBParameterGroupName :: Text
      -- ^ The name of the DB parameter group. Constraints: Must be the name
      -- of an existing DB parameter group Must be 1 to 255 alphanumeric
      -- characters First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens.
    , _mdbpgmParameters :: [Parameter]
      -- ^ An array of parameter names, values, and the apply method for the
      -- parameter update. At least one parameter name, value, and apply
      -- method must be supplied; subsequent arguments are optional. A
      -- maximum of 20 parameters may be modified in a single request.
      -- Valid Values (for the application method): immediate |
      -- pending-reboot You can use the immediate value with dynamic
      -- parameters only. You can use the pending-reboot value for both
      -- dynamic and static parameters, and changes are applied when DB
      -- instance reboots.
    } deriving (Show, Generic)

-- | The name of the DB parameter group. Constraints: Must be the name of an
-- existing DB parameter group Must be 1 to 255 alphanumeric characters First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens.
mdbpgmDBParameterGroupName :: Lens' ModifyDBParameterGroup (Text)
mdbpgmDBParameterGroupName = lens _mdbpgmDBParameterGroupName (\s a -> s { _mdbpgmDBParameterGroupName = a })
{-# INLINE mdbpgmDBParameterGroupName #-}

-- | An array of parameter names, values, and the apply method for the parameter
-- update. At least one parameter name, value, and apply method must be
-- supplied; subsequent arguments are optional. A maximum of 20 parameters may
-- be modified in a single request. Valid Values (for the application method):
-- immediate | pending-reboot You can use the immediate value with dynamic
-- parameters only. You can use the pending-reboot value for both dynamic and
-- static parameters, and changes are applied when DB instance reboots.
mdbpgmParameters :: Lens' ModifyDBParameterGroup ([Parameter])
mdbpgmParameters = lens _mdbpgmParameters (\s a -> s { _mdbpgmParameters = a })
{-# INLINE mdbpgmParameters #-}

instance ToQuery ModifyDBParameterGroup where
    toQuery = genericQuery def

newtype ModifyDBParameterGroupResponse = ModifyDBParameterGroupResponse
    { _dbpgnmDBParameterGroupName :: Maybe Text
      -- ^ The name of the DB parameter group.
    } deriving (Show, Generic)

-- | The name of the DB parameter group.
dbpgnmDBParameterGroupName :: Lens' ModifyDBParameterGroupResponse (Maybe Text)
dbpgnmDBParameterGroupName = lens _dbpgnmDBParameterGroupName (\s a -> s { _dbpgnmDBParameterGroupName = a })
{-# INLINE dbpgnmDBParameterGroupName #-}

instance FromXML ModifyDBParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyDBParameterGroup where
    type Sv ModifyDBParameterGroup = RDS
    type Rs ModifyDBParameterGroup = ModifyDBParameterGroupResponse

    request = post "ModifyDBParameterGroup"
    response _ = xmlResponse
