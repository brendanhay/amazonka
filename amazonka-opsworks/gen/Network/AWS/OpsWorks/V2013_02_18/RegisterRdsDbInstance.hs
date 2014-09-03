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
    , registerRdsDbInstance
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

-- | Minimum specification for a 'RegisterRdsDbInstance' request.
registerRdsDbInstance :: Text -- ^ 'rrdirStackId'
                      -> Text -- ^ 'rrdirRdsDbInstanceArn'
                      -> Text -- ^ 'rrdirDbUser'
                      -> Text -- ^ 'rrdirDbPassword'
                      -> RegisterRdsDbInstance
registerRdsDbInstance p1 p2 p3 p4 = RegisterRdsDbInstance
    { _rrdirStackId = p1
    , _rrdirRdsDbInstanceArn = p2
    , _rrdirDbUser = p3
    , _rrdirDbPassword = p4
    }

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
rrdirStackId
    :: Functor f
    => (Text
    -> f (Text))
    -> RegisterRdsDbInstance
    -> f RegisterRdsDbInstance
rrdirStackId f x =
    (\y -> x { _rrdirStackId = y })
       <$> f (_rrdirStackId x)
{-# INLINE rrdirStackId #-}

-- | The Amazon RDS instance's ARN.
rrdirRdsDbInstanceArn
    :: Functor f
    => (Text
    -> f (Text))
    -> RegisterRdsDbInstance
    -> f RegisterRdsDbInstance
rrdirRdsDbInstanceArn f x =
    (\y -> x { _rrdirRdsDbInstanceArn = y })
       <$> f (_rrdirRdsDbInstanceArn x)
{-# INLINE rrdirRdsDbInstanceArn #-}

-- | The database's master user name.
rrdirDbUser
    :: Functor f
    => (Text
    -> f (Text))
    -> RegisterRdsDbInstance
    -> f RegisterRdsDbInstance
rrdirDbUser f x =
    (\y -> x { _rrdirDbUser = y })
       <$> f (_rrdirDbUser x)
{-# INLINE rrdirDbUser #-}

-- | The database password.
rrdirDbPassword
    :: Functor f
    => (Text
    -> f (Text))
    -> RegisterRdsDbInstance
    -> f RegisterRdsDbInstance
rrdirDbPassword f x =
    (\y -> x { _rrdirDbPassword = y })
       <$> f (_rrdirDbPassword x)
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
