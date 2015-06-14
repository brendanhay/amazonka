{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.ResetDBParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modifies the parameters of a DB parameter group to the engine\/system
-- default value. To reset specific parameters submit a list of the
-- following: @ParameterName@ and @ApplyMethod@. To reset the entire DB
-- parameter group, specify the @DBParameterGroup@ name and
-- @ResetAllParameters@ parameters. When resetting the entire group,
-- dynamic parameters are updated immediately and static parameters are set
-- to @pending-reboot@ to take effect on the next DB instance restart or
-- @RebootDBInstance@ request.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ResetDBParameterGroup.html>
module Network.AWS.RDS.ResetDBParameterGroup
    (
    -- * Request
      ResetDBParameterGroup
    -- ** Request constructor
    , resetDBParameterGroup
    -- ** Request lenses
    , rdpgResetAllParameters
    , rdpgParameters
    , rdpgDBParameterGroupName

    -- * Response
    , DBParameterGroupNameMessage
    -- ** Response constructor
    , dbParameterGroupNameMessage
    -- ** Response lenses
    , dpgnmDBParameterGroupName
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.RDS.Types

-- | /See:/ 'resetDBParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdpgResetAllParameters'
--
-- * 'rdpgParameters'
--
-- * 'rdpgDBParameterGroupName'
data ResetDBParameterGroup = ResetDBParameterGroup'{_rdpgResetAllParameters :: Maybe Bool, _rdpgParameters :: [Parameter], _rdpgDBParameterGroupName :: Text} deriving (Eq, Read, Show)

-- | 'ResetDBParameterGroup' smart constructor.
resetDBParameterGroup :: Text -> ResetDBParameterGroup
resetDBParameterGroup pDBParameterGroupName = ResetDBParameterGroup'{_rdpgResetAllParameters = Nothing, _rdpgParameters = mempty, _rdpgDBParameterGroupName = pDBParameterGroupName};

-- | Specifies whether (@true@) or not (@false@) to reset all parameters in
-- the DB parameter group to default values.
--
-- Default: @true@
rdpgResetAllParameters :: Lens' ResetDBParameterGroup (Maybe Bool)
rdpgResetAllParameters = lens _rdpgResetAllParameters (\ s a -> s{_rdpgResetAllParameters = a});

-- | An array of parameter names, values, and the apply method for the
-- parameter update. At least one parameter name, value, and apply method
-- must be supplied; subsequent arguments are optional. A maximum of 20
-- parameters may be modified in a single request.
--
-- __MySQL__
--
-- Valid Values (for Apply method): @immediate@ | @pending-reboot@
--
-- You can use the immediate value with dynamic parameters only. You can
-- use the @pending-reboot@ value for both dynamic and static parameters,
-- and changes are applied when DB instance reboots.
--
-- __Oracle__
--
-- Valid Values (for Apply method): @pending-reboot@
rdpgParameters :: Lens' ResetDBParameterGroup [Parameter]
rdpgParameters = lens _rdpgParameters (\ s a -> s{_rdpgParameters = a});

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
rdpgDBParameterGroupName :: Lens' ResetDBParameterGroup Text
rdpgDBParameterGroupName = lens _rdpgDBParameterGroupName (\ s a -> s{_rdpgDBParameterGroupName = a});

instance AWSRequest ResetDBParameterGroup where
        type Sv ResetDBParameterGroup = RDS
        type Rs ResetDBParameterGroup =
             DBParameterGroupNameMessage
        request = post
        response
          = receiveXMLWrapper "ResetDBParameterGroupResult"
              (\ s h x -> parseXML x)

instance ToHeaders ResetDBParameterGroup where
        toHeaders = const mempty

instance ToPath ResetDBParameterGroup where
        toPath = const "/"

instance ToQuery ResetDBParameterGroup where
        toQuery ResetDBParameterGroup'{..}
          = mconcat
              ["Action" =: ("ResetDBParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ResetAllParameters" =: _rdpgResetAllParameters,
               "Parameters" =: "Parameter" =: _rdpgParameters,
               "DBParameterGroupName" =: _rdpgDBParameterGroupName]
