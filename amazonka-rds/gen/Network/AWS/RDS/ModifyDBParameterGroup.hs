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
module Network.AWS.RDS.ModifyDBParameterGroup
    (
    -- * Request
      ModifyDBParameterGroupMessage
    -- ** Request constructor
    , modifyDBParameterGroupMessage
    -- ** Request lenses
    , mdbpgmDBParameterGroupName
    , mdbpgmParameters

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

data ModifyDBParameterGroupMessage = ModifyDBParameterGroupMessage
    { _mdbpgmDBParameterGroupName :: Text
    , _mdbpgmParameters           :: [Parameter]
    } deriving (Eq, Show, Generic)

-- | 'ModifyDBParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdbpgmDBParameterGroupName' @::@ 'Text'
--
-- * 'mdbpgmParameters' @::@ ['Parameter']
--
modifyDBParameterGroupMessage :: Text -- ^ 'mdbpgmDBParameterGroupName'
                              -> ModifyDBParameterGroupMessage
modifyDBParameterGroupMessage p1 = ModifyDBParameterGroupMessage
    { _mdbpgmDBParameterGroupName = p1
    , _mdbpgmParameters           = mempty
    }

-- | The name of the DB parameter group. Constraints: Must be the name of an
-- existing DB parameter group Must be 1 to 255 alphanumeric characters
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens.
mdbpgmDBParameterGroupName :: Lens' ModifyDBParameterGroupMessage Text
mdbpgmDBParameterGroupName =
    lens _mdbpgmDBParameterGroupName
        (\s a -> s { _mdbpgmDBParameterGroupName = a })

-- | An array of parameter names, values, and the apply method for the
-- parameter update. At least one parameter name, value, and apply method
-- must be supplied; subsequent arguments are optional. A maximum of 20
-- parameters may be modified in a single request. Valid Values (for the
-- application method): immediate | pending-reboot.
mdbpgmParameters :: Lens' ModifyDBParameterGroupMessage [Parameter]
mdbpgmParameters = lens _mdbpgmParameters (\s a -> s { _mdbpgmParameters = a })

instance ToPath ModifyDBParameterGroupMessage where
    toPath = const "/"

instance ToQuery ModifyDBParameterGroupMessage

instance AWSRequest ModifyDBParameterGroupMessage where
    type Sv ModifyDBParameterGroupMessage = RDS
    type Rs ModifyDBParameterGroupMessage = DBParameterGroupNameMessage

    request  = post "ModifyDBParameterGroup"
    response = const . xmlResponse $ const decodeCursor
