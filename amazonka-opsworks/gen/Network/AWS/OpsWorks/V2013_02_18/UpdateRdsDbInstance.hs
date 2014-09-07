{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UpdateRdsDbInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates an Amazon RDS instance.
module Network.AWS.OpsWorks.V2013_02_18.UpdateRdsDbInstance
    (
    -- * Request
      UpdateRdsDbInstance
    -- ** Request constructor
    , mkUpdateRdsDbInstance
    -- ** Request lenses
    , urdiRdsDbInstanceArn
    , urdiDbUser
    , urdiDbPassword

    -- * Response
    , UpdateRdsDbInstanceResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data UpdateRdsDbInstance = UpdateRdsDbInstance
    { _urdiRdsDbInstanceArn :: Text
    , _urdiDbUser :: Maybe Text
    , _urdiDbPassword :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateRdsDbInstance' request.
mkUpdateRdsDbInstance :: Text -- ^ 'urdiRdsDbInstanceArn'
                      -> UpdateRdsDbInstance
mkUpdateRdsDbInstance p1 = UpdateRdsDbInstance
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

instance AWSRequest UpdateRdsDbInstance where
    type Sv UpdateRdsDbInstance = OpsWorks
    type Rs UpdateRdsDbInstance = UpdateRdsDbInstanceResponse

    request = get
    response _ = nullaryResponse UpdateRdsDbInstanceResponse
