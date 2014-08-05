{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.RebootInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Reboots a specified instance. For more information, see Starting, Stopping,
-- and Rebooting Instances. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.RebootInstance where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

data RebootInstance = RebootInstance
    { _rirInstanceId :: Text
      -- ^ The instance ID.
    } deriving (Show, Generic)

makeLenses ''RebootInstance

instance ToPath RebootInstance

instance ToQuery RebootInstance

instance ToHeaders RebootInstance

instance ToJSON RebootInstance

data RebootInstanceResponse = RebootInstanceResponse
    deriving (Eq, Show, Generic)

makeLenses ''RebootInstanceResponse

instance AWSRequest RebootInstance where
    type Sv RebootInstance = OpsWorks
    type Rs RebootInstance = RebootInstanceResponse

    request = get
    response _ _ = return (Right RebootInstanceResponse)
