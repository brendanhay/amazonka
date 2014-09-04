{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.RegisterRdsDbInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers an Amazon RDS instance with a stack.
module Network.AWS.OpsWorks.V2013_02_18.RegisterRdsDbInstance
    (
    -- * Request
      RegisterRdsDbInstance
    -- ** Request constructor
    , mkRegisterRdsDbInstanceRequest
    -- ** Request lenses
    , rrdirStackId
    , rrdirRdsDbInstanceArn
    , rrdirDbUser
    , rrdirDbPassword

    -- * Response
    , RegisterRdsDbInstanceResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterRdsDbInstance' request.
mkRegisterRdsDbInstanceRequest :: Text -- ^ 'rrdirStackId'
                               -> Text -- ^ 'rrdirRdsDbInstanceArn'
                               -> Text -- ^ 'rrdirDbUser'
                               -> Text -- ^ 'rrdirDbPassword'
                               -> RegisterRdsDbInstance
mkRegisterRdsDbInstanceRequest p1 p2 p3 p4 = RegisterRdsDbInstance
    { _rrdirStackId = p1
    , _rrdirRdsDbInstanceArn = p2
    , _rrdirDbUser = p3
    , _rrdirDbPassword = p4
    }
{-# INLINE mkRegisterRdsDbInstanceRequest #-}

data RegisterRdsDbInstance = RegisterRdsDbInstance
    { _rrdirStackId :: Text
      -- ^ The stack ID.
    , _rrdirRdsDbInstanceArn :: Text
      -- ^ The Amazon RDS instance's ARN.
    , _rrdirDbUser :: Text
      -- ^ The database's master user name.
    , _rrdirDbPassword :: Text
      -- ^ The database password.
    } deriving (Show, Generic)

-- | The stack ID.
rrdirStackId :: Lens' RegisterRdsDbInstance (Text)
rrdirStackId = lens _rrdirStackId (\s a -> s { _rrdirStackId = a })
{-# INLINE rrdirStackId #-}

-- | The Amazon RDS instance's ARN.
rrdirRdsDbInstanceArn :: Lens' RegisterRdsDbInstance (Text)
rrdirRdsDbInstanceArn = lens _rrdirRdsDbInstanceArn (\s a -> s { _rrdirRdsDbInstanceArn = a })
{-# INLINE rrdirRdsDbInstanceArn #-}

-- | The database's master user name.
rrdirDbUser :: Lens' RegisterRdsDbInstance (Text)
rrdirDbUser = lens _rrdirDbUser (\s a -> s { _rrdirDbUser = a })
{-# INLINE rrdirDbUser #-}

-- | The database password.
rrdirDbPassword :: Lens' RegisterRdsDbInstance (Text)
rrdirDbPassword = lens _rrdirDbPassword (\s a -> s { _rrdirDbPassword = a })
{-# INLINE rrdirDbPassword #-}

instance ToPath RegisterRdsDbInstance

instance ToQuery RegisterRdsDbInstance

instance ToHeaders RegisterRdsDbInstance

instance ToJSON RegisterRdsDbInstance

data RegisterRdsDbInstanceResponse = RegisterRdsDbInstanceResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RegisterRdsDbInstance where
    type Sv RegisterRdsDbInstance = OpsWorks
    type Rs RegisterRdsDbInstance = RegisterRdsDbInstanceResponse

    request = get
    response _ = nullaryResponse RegisterRdsDbInstanceResponse
