{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.ModifyDBParameterGroup
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

-- | Modifies the parameters of a DB parameter group. To modify more than one
-- parameter, submit a list of the following: @ParameterName@,
-- @ParameterValue@, and @ApplyMethod@. A maximum of 20 parameters can be
-- modified in a single request.
--
-- Changes to dynamic parameters are applied immediately. Changes to static
-- parameters require a reboot without failover to the DB instance
-- associated with the parameter group before the change can take effect.
--
-- After you modify a DB parameter group, you should wait at least 5
-- minutes before creating your first DB instance that uses that DB
-- parameter group as the default parameter group. This allows Amazon RDS
-- to fully complete the modify action before the parameter group is used
-- as the default for a new DB instance. This is especially important for
-- parameters that are critical when creating the default database for a DB
-- instance, such as the character set for the default database defined by
-- the @character_set_database@ parameter. You can use the /Parameter
-- Groups/ option of the
-- <https://console.aws.amazon.com/rds/ Amazon RDS console> or the
-- /DescribeDBParameters/ command to verify that your DB parameter group
-- has been created or modified.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyDBParameterGroup.html>
module Network.AWS.RDS.ModifyDBParameterGroup
    (
    -- * Request
      ModifyDBParameterGroup
    -- ** Request constructor
    , modifyDBParameterGroup
    -- ** Request lenses
    , mdpgDBParameterGroupName
    , mdpgParameters

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

-- | /See:/ 'modifyDBParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdpgDBParameterGroupName'
--
-- * 'mdpgParameters'
data ModifyDBParameterGroup = ModifyDBParameterGroup'{_mdpgDBParameterGroupName :: Text, _mdpgParameters :: [Parameter]} deriving (Eq, Read, Show)

-- | 'ModifyDBParameterGroup' smart constructor.
modifyDBParameterGroup :: Text -> [Parameter] -> ModifyDBParameterGroup
modifyDBParameterGroup pDBParameterGroupName pParameters = ModifyDBParameterGroup'{_mdpgDBParameterGroupName = pDBParameterGroupName, _mdpgParameters = pParameters};

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be the name of an existing DB parameter group
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
mdpgDBParameterGroupName :: Lens' ModifyDBParameterGroup Text
mdpgDBParameterGroupName = lens _mdpgDBParameterGroupName (\ s a -> s{_mdpgDBParameterGroupName = a});

-- | An array of parameter names, values, and the apply method for the
-- parameter update. At least one parameter name, value, and apply method
-- must be supplied; subsequent arguments are optional. A maximum of 20
-- parameters may be modified in a single request.
--
-- Valid Values (for the application method): @immediate | pending-reboot@
--
-- You can use the immediate value with dynamic parameters only. You can
-- use the pending-reboot value for both dynamic and static parameters, and
-- changes are applied when you reboot the DB instance without failover.
mdpgParameters :: Lens' ModifyDBParameterGroup [Parameter]
mdpgParameters = lens _mdpgParameters (\ s a -> s{_mdpgParameters = a});

instance AWSRequest ModifyDBParameterGroup where
        type Sv ModifyDBParameterGroup = RDS
        type Rs ModifyDBParameterGroup =
             DBParameterGroupNameMessage
        request = post
        response
          = receiveXMLWrapper "ModifyDBParameterGroupResult"
              (\ s h x -> parseXML x)

instance ToHeaders ModifyDBParameterGroup where
        toHeaders = const mempty

instance ToPath ModifyDBParameterGroup where
        toPath = const "/"

instance ToQuery ModifyDBParameterGroup where
        toQuery ModifyDBParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("ModifyDBParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBParameterGroupName" =: _mdpgDBParameterGroupName,
               "Parameters" =: "Parameter" =: _mdpgParameters]
