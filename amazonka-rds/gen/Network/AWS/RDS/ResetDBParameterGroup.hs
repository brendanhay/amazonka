{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.RDS.ResetDBParameterGroup
    (
    -- * Request
      ResetDBParameterGroup
    -- ** Request constructor
    , mkResetDBParameterGroup
    -- ** Request lenses
    , rdbpgDBParameterGroupName
    , rdbpgResetAllParameters
    , rdbpgParameters

    -- * Response
    , ResetDBParameterGroupResponse
    -- ** Response constructor
    , mkResetDBParameterGroupResponse
    -- ** Response lenses
    , rdbpgrDBParameterGroupName
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data ResetDBParameterGroup = ResetDBParameterGroup
    { _rdbpgDBParameterGroupName :: Text
    , _rdbpgResetAllParameters :: Maybe Bool
    , _rdbpgParameters :: [Parameter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetDBParameterGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroupName ::@ @Text@
--
-- * @ResetAllParameters ::@ @Maybe Bool@
--
-- * @Parameters ::@ @[Parameter]@
--
mkResetDBParameterGroup :: Text -- ^ 'rdbpgDBParameterGroupName'
                        -> ResetDBParameterGroup
mkResetDBParameterGroup p1 = ResetDBParameterGroup
    { _rdbpgDBParameterGroupName = p1
    , _rdbpgResetAllParameters = Nothing
    , _rdbpgParameters = mempty
    }

-- | The name of the DB parameter group. Constraints: Must be 1 to 255
-- alphanumeric characters First character must be a letter Cannot end with a
-- hyphen or contain two consecutive hyphens.
rdbpgDBParameterGroupName :: Lens' ResetDBParameterGroup Text
rdbpgDBParameterGroupName =
    lens _rdbpgDBParameterGroupName
         (\s a -> s { _rdbpgDBParameterGroupName = a })

-- | Specifies whether (true) or not (false) to reset all parameters in the DB
-- parameter group to default values. Default: true.
rdbpgResetAllParameters :: Lens' ResetDBParameterGroup (Maybe Bool)
rdbpgResetAllParameters =
    lens _rdbpgResetAllParameters
         (\s a -> s { _rdbpgResetAllParameters = a })

-- | An array of parameter names, values, and the apply method for the parameter
-- update. At least one parameter name, value, and apply method must be
-- supplied; subsequent arguments are optional. A maximum of 20 parameters may
-- be modified in a single request. MySQL Valid Values (for Apply method):
-- immediate | pending-reboot You can use the immediate value with dynamic
-- parameters only. You can use the pending-reboot value for both dynamic and
-- static parameters, and changes are applied when DB instance reboots. Oracle
-- Valid Values (for Apply method): pending-reboot.
rdbpgParameters :: Lens' ResetDBParameterGroup [Parameter]
rdbpgParameters = lens _rdbpgParameters (\s a -> s { _rdbpgParameters = a })

instance ToQuery ResetDBParameterGroup where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- ModifyDBParameterGroup or ResetDBParameterGroup action.
newtype ResetDBParameterGroupResponse = ResetDBParameterGroupResponse
    { _rdbpgrDBParameterGroupName :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResetDBParameterGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBParameterGroupName ::@ @Maybe Text@
--
mkResetDBParameterGroupResponse :: ResetDBParameterGroupResponse
mkResetDBParameterGroupResponse = ResetDBParameterGroupResponse
    { _rdbpgrDBParameterGroupName = Nothing
    }

-- | The name of the DB parameter group.
rdbpgrDBParameterGroupName :: Lens' ResetDBParameterGroupResponse (Maybe Text)
rdbpgrDBParameterGroupName =
    lens _rdbpgrDBParameterGroupName
         (\s a -> s { _rdbpgrDBParameterGroupName = a })

instance FromXML ResetDBParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ResetDBParameterGroup where
    type Sv ResetDBParameterGroup = RDS
    type Rs ResetDBParameterGroup = ResetDBParameterGroupResponse

    request = post "ResetDBParameterGroup"
    response _ = xmlResponse
