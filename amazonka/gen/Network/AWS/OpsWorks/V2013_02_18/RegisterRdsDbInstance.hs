{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.OpsWorks.V2013_02_18.RegisterRdsDbInstance where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

data RegisterRdsDbInstance = RegisterRdsDbInstance
    { _rrdirRdsDbInstanceArn :: Text
      -- ^ The Amazon RDS instance's ARN.
    , _rrdirDbUser :: Text
      -- ^ The database's master user name.
    , _rrdirStackId :: Text
      -- ^ The stack ID.
    , _rrdirDbPassword :: Text
      -- ^ The database password.
    } deriving (Show, Generic)

makeLenses ''RegisterRdsDbInstance

instance ToPath RegisterRdsDbInstance

instance ToQuery RegisterRdsDbInstance

instance ToHeaders RegisterRdsDbInstance

instance ToJSON RegisterRdsDbInstance

data RegisterRdsDbInstanceResponse = RegisterRdsDbInstanceResponse
    deriving (Eq, Show, Generic)

makeLenses ''RegisterRdsDbInstanceResponse

instance AWSRequest RegisterRdsDbInstance where
    type Sv RegisterRdsDbInstance = OpsWorks
    type Rs RegisterRdsDbInstance = RegisterRdsDbInstanceResponse

    request = get
    response _ = nullaryResponse RegisterRdsDbInstanceResponse
