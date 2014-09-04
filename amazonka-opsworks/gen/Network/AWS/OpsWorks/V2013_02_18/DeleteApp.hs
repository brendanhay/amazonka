{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.OpsWorks.V2013_02_18.DeleteApp
    (
    -- * Request
      DeleteApp
    -- ** Request constructor
    , mkDeleteAppRequest
    -- ** Request lenses
    , darAppId

    -- * Response
    , DeleteAppResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteApp' request.
mkDeleteAppRequest :: Text -- ^ 'darAppId'
                   -> DeleteApp
mkDeleteAppRequest p1 = DeleteApp
    { _darAppId = p1
    }
{-# INLINE mkDeleteAppRequest #-}

newtype DeleteApp = DeleteApp
    { _darAppId :: Text
      -- ^ The app ID.
    } deriving (Show, Generic)

-- | The app ID.
darAppId :: Lens' DeleteApp (Text)
darAppId = lens _darAppId (\s a -> s { _darAppId = a })
{-# INLINE darAppId #-}

instance ToPath DeleteApp

instance ToQuery DeleteApp

instance ToHeaders DeleteApp

instance ToJSON DeleteApp

data DeleteAppResponse = DeleteAppResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteApp where
    type Sv DeleteApp = OpsWorks
    type Rs DeleteApp = DeleteAppResponse

    request = get
    response _ = nullaryResponse DeleteAppResponse
