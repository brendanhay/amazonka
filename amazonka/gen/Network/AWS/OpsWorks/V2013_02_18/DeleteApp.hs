{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DeleteApp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified app. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DeleteApp where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude

data DeleteApp = DeleteApp
    { _darAppId :: Text
      -- ^ The app ID.
    } deriving (Show, Generic)

makeLenses ''DeleteApp

instance ToPath DeleteApp

instance ToQuery DeleteApp

instance ToHeaders DeleteApp

instance ToJSON DeleteApp

data DeleteAppResponse = DeleteAppResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteAppResponse

instance AWSRequest DeleteApp where
    type Sv DeleteApp = OpsWorks
    type Rs DeleteApp = DeleteAppResponse

    request = get
    response _ = nullaryResponse DeleteAppResponse
