{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.DeleteStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified stack. You must first delete all instances, layers, and
-- apps. For more information, see Shut Down a Stack. Required Permissions: To
-- use this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.DeleteStack
    (
    -- * Request
      DeleteStack
    -- ** Request constructor
    , mkDeleteStack
    -- ** Request lenses
    , ds1StackId

    -- * Response
    , DeleteStackResponse
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype DeleteStack = DeleteStack
    { _ds1StackId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteStack' request.
mkDeleteStack :: Text -- ^ 'ds1StackId'
              -> DeleteStack
mkDeleteStack p1 = DeleteStack
    { _ds1StackId = p1
    }

-- | The stack ID.
ds1StackId :: Lens' DeleteStack Text
ds1StackId = lens _ds1StackId (\s a -> s { _ds1StackId = a })

instance ToPath DeleteStack

instance ToQuery DeleteStack

instance ToHeaders DeleteStack

instance ToJSON DeleteStack

data DeleteStackResponse = DeleteStackResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteStack where
    type Sv DeleteStack = OpsWorks
    type Rs DeleteStack = DeleteStackResponse

    request = get
    response _ = nullaryResponse DeleteStackResponse
