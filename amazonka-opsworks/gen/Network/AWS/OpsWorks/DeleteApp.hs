{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.DeleteApp
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
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteApp.html>
module Network.AWS.OpsWorks.DeleteApp
    (
    -- * Request
      DeleteApp
    -- ** Request constructor
    , deleteApp
    -- ** Request lenses
    , daAppId

    -- * Response
    , DeleteAppResponse
    -- ** Response constructor
    , deleteAppResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DeleteApp = DeleteApp
    { _daAppId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteApp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daAppId' @::@ 'Text'
--
deleteApp :: Text -- ^ 'daAppId'
          -> DeleteApp
deleteApp p1 = DeleteApp
    { _daAppId = p1
    }

-- | The app ID.
daAppId :: Lens' DeleteApp Text
daAppId = lens _daAppId (\s a -> s { _daAppId = a })

data DeleteAppResponse = DeleteAppResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteAppResponse' constructor.
deleteAppResponse :: DeleteAppResponse
deleteAppResponse = DeleteAppResponse

instance ToPath DeleteApp where
    toPath = const "/"

instance ToQuery DeleteApp where
    toQuery = const mempty

instance ToHeaders DeleteApp
instance ToJSON DeleteApp where
    toJSON = genericToJSON jsonOptions

instance AWSRequest DeleteApp where
    type Sv DeleteApp = OpsWorks
    type Rs DeleteApp = DeleteAppResponse

    request  = post "DeleteApp"
    response = nullResponse DeleteAppResponse
