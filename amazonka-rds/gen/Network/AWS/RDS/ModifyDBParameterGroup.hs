{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.RDS.ModifyDBParameterGroup
    (
    -- * Request
      ModifyDBParameterGroup
    -- ** Request constructor
    , mkModifyDBParameterGroup
    -- ** Request lenses
    , mdbpgDBParameterGroupName
    , mdbpgParameters

    -- * Response
    , ModifyDBParameterGroupResponse
    -- ** Response constructor
    , mkModifyDBParameterGroupResponse
    -- ** Response lenses
    , mdbpgrDBParameterGroupName
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data ModifyDBParameterGroup = ModifyDBParameterGroup
    { _mdbpgDBParameterGroupName :: !Text
    , _mdbpgParameters :: [Parameter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyDBParameterGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroupName ::@ @Text@
--
-- * @Parameters ::@ @[Parameter]@
--
mkModifyDBParameterGroup :: Text -- ^ 'mdbpgDBParameterGroupName'
                         -> [Parameter] -- ^ 'mdbpgParameters'
                         -> ModifyDBParameterGroup
mkModifyDBParameterGroup p1 p2 = ModifyDBParameterGroup
    { _mdbpgDBParameterGroupName = p1
    , _mdbpgParameters = p2
    }

-- | The name of the DB parameter group. Constraints: Must be the name of an
-- existing DB parameter group Must be 1 to 255 alphanumeric characters First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens.
mdbpgDBParameterGroupName :: Lens' ModifyDBParameterGroup Text
mdbpgDBParameterGroupName =
    lens _mdbpgDBParameterGroupName
         (\s a -> s { _mdbpgDBParameterGroupName = a })

-- | An array of parameter names, values, and the apply method for the parameter
-- update. At least one parameter name, value, and apply method must be
-- supplied; subsequent arguments are optional. A maximum of 20 parameters may
-- be modified in a single request. Valid Values (for the application method):
-- immediate | pending-reboot You can use the immediate value with dynamic
-- parameters only. You can use the pending-reboot value for both dynamic and
-- static parameters, and changes are applied when DB instance reboots.
mdbpgParameters :: Lens' ModifyDBParameterGroup [Parameter]
mdbpgParameters = lens _mdbpgParameters (\s a -> s { _mdbpgParameters = a })

instance ToQuery ModifyDBParameterGroup where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- ModifyDBParameterGroup or ResetDBParameterGroup action.
newtype ModifyDBParameterGroupResponse = ModifyDBParameterGroupResponse
    { _mdbpgrDBParameterGroupName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyDBParameterGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroupName ::@ @Maybe Text@
--
mkModifyDBParameterGroupResponse :: ModifyDBParameterGroupResponse
mkModifyDBParameterGroupResponse = ModifyDBParameterGroupResponse
    { _mdbpgrDBParameterGroupName = Nothing
    }

-- | The name of the DB parameter group.
mdbpgrDBParameterGroupName :: Lens' ModifyDBParameterGroupResponse (Maybe Text)
mdbpgrDBParameterGroupName =
    lens _mdbpgrDBParameterGroupName
         (\s a -> s { _mdbpgrDBParameterGroupName = a })

instance FromXML ModifyDBParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyDBParameterGroup where
    type Sv ModifyDBParameterGroup = RDS
    type Rs ModifyDBParameterGroup = ModifyDBParameterGroupResponse

    request = post "ModifyDBParameterGroup"
    response _ = xmlResponse
