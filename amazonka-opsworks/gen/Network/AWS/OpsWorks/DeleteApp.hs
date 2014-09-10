{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks
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
module Network.AWS.OpsWorks
    (
    -- * Request
      DeleteApp
    -- ** Request constructor
    , mkDeleteApp
    -- ** Request lenses
    , daAppId

    -- * Response
    , DeleteAppResponse
    -- ** Response constructor
    , mkDeleteAppResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DeleteApp = DeleteApp
    { _daAppId :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteApp' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AppId ::@ @Text@
--
mkDeleteApp :: Text -- ^ 'daAppId'
            -> DeleteApp
mkDeleteApp p1 = DeleteApp
    { _daAppId = p1
    }

-- | The app ID.
daAppId :: Lens' DeleteApp Text
daAppId = lens _daAppId (\s a -> s { _daAppId = a })

instance ToPath DeleteApp

instance ToQuery DeleteApp

instance ToHeaders DeleteApp

instance ToJSON DeleteApp

data DeleteAppResponse = DeleteAppResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAppResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteAppResponse :: DeleteAppResponse
mkDeleteAppResponse = DeleteAppResponse

instance AWSRequest DeleteApp where
    type Sv DeleteApp = OpsWorks
    type Rs DeleteApp = DeleteAppResponse

    request = get
    response _ = nullaryResponse DeleteAppResponse
