{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.UpdateRdsDbInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates an Amazon RDS instance. Required Permissions: To use this action,
-- an IAM user must have a Manage permissions level for the stack, or an
-- attached policy that explicitly grants permissions. For more information on
-- user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.UpdateRdsDbInstance
    (
    -- * Request
      UpdateRdsDbInstance
    -- ** Request constructor
    , updateRdsDbInstance
    -- ** Request lenses
    , urdiRdsDbInstanceArn
    , urdiDbUser
    , urdiDbPassword

    -- * Response
    , UpdateRdsDbInstanceResponse
    -- ** Response constructor
    , updateRdsDbInstanceResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data UpdateRdsDbInstance = UpdateRdsDbInstance
    { _urdiRdsDbInstanceArn :: Text
    , _urdiDbUser :: Maybe Text
    , _urdiDbPassword :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateRdsDbInstance' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RdsDbInstanceArn ::@ @Text@
--
-- * @DbUser ::@ @Maybe Text@
--
-- * @DbPassword ::@ @Maybe Text@
--
updateRdsDbInstance :: Text -- ^ 'urdiRdsDbInstanceArn'
                    -> UpdateRdsDbInstance
updateRdsDbInstance p1 = UpdateRdsDbInstance
    { _urdiRdsDbInstanceArn = p1
    , _urdiDbUser = Nothing
    , _urdiDbPassword = Nothing
    }

-- | The Amazon RDS instance's ARN.
urdiRdsDbInstanceArn :: Lens' UpdateRdsDbInstance Text
urdiRdsDbInstanceArn =
    lens _urdiRdsDbInstanceArn (\s a -> s { _urdiRdsDbInstanceArn = a })

-- | The master user name.
urdiDbUser :: Lens' UpdateRdsDbInstance (Maybe Text)
urdiDbUser = lens _urdiDbUser (\s a -> s { _urdiDbUser = a })

-- | The database password.
urdiDbPassword :: Lens' UpdateRdsDbInstance (Maybe Text)
urdiDbPassword = lens _urdiDbPassword (\s a -> s { _urdiDbPassword = a })

instance ToPath UpdateRdsDbInstance

instance ToQuery UpdateRdsDbInstance

instance ToHeaders UpdateRdsDbInstance

instance ToJSON UpdateRdsDbInstance

data UpdateRdsDbInstanceResponse = UpdateRdsDbInstanceResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateRdsDbInstanceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
updateRdsDbInstanceResponse :: UpdateRdsDbInstanceResponse
updateRdsDbInstanceResponse = UpdateRdsDbInstanceResponse

instance AWSRequest UpdateRdsDbInstance where
    type Sv UpdateRdsDbInstance = OpsWorks
    type Rs UpdateRdsDbInstance = UpdateRdsDbInstanceResponse

    request = get
    response _ = nullaryResponse UpdateRdsDbInstanceResponse
