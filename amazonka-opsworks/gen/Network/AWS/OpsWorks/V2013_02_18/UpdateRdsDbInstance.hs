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
    , mkUpdateRdsDbInstanceRequest
    -- ** Request lenses
    , urdirRdsDbInstanceArn
    , urdirDbUser
    , urdirDbPassword

    -- * Response
    , UpdateRdsDbInstanceResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateRdsDbInstance' request.
mkUpdateRdsDbInstanceRequest :: Text -- ^ 'urdirRdsDbInstanceArn'
                             -> UpdateRdsDbInstance
mkUpdateRdsDbInstanceRequest p1 = UpdateRdsDbInstance
    { _urdirRdsDbInstanceArn = p1
    , _urdirDbUser = Nothing
    , _urdirDbPassword = Nothing
    }
{-# INLINE mkUpdateRdsDbInstanceRequest #-}

data UpdateRdsDbInstance = UpdateRdsDbInstance
    { _urdirRdsDbInstanceArn :: Text
      -- ^ The Amazon RDS instance's ARN.
    , _urdirDbUser :: Maybe Text
      -- ^ The master user name.
    , _urdirDbPassword :: Maybe Text
      -- ^ The database password.
    } deriving (Show, Generic)

-- | The Amazon RDS instance's ARN.
urdirRdsDbInstanceArn :: Lens' UpdateRdsDbInstance (Text)
urdirRdsDbInstanceArn = lens _urdirRdsDbInstanceArn (\s a -> s { _urdirRdsDbInstanceArn = a })
{-# INLINE urdirRdsDbInstanceArn #-}

-- | The master user name.
urdirDbUser :: Lens' UpdateRdsDbInstance (Maybe Text)
urdirDbUser = lens _urdirDbUser (\s a -> s { _urdirDbUser = a })
{-# INLINE urdirDbUser #-}

-- | The database password.
urdirDbPassword :: Lens' UpdateRdsDbInstance (Maybe Text)
urdirDbPassword = lens _urdirDbPassword (\s a -> s { _urdirDbPassword = a })
{-# INLINE urdirDbPassword #-}

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
