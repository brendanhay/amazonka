{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.StopStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Stops a specified stack. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.StopStack
    (
    -- * Request
      StopStack
    -- ** Request constructor
    , mkStopStack
    -- ** Request lenses
    , ss2StackId

    -- * Response
    , StopStackResponse
    -- ** Response constructor
    , mkStopStackResponse
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype StopStack = StopStack
    { _ss2StackId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StopStack' request.
mkStopStack :: Text -- ^ 'ss2StackId'
            -> StopStack
mkStopStack p1 = StopStack
    { _ss2StackId = p1
    }

-- | The stack ID.
ss2StackId :: Lens' StopStack Text
ss2StackId = lens _ss2StackId (\s a -> s { _ss2StackId = a })

instance ToPath StopStack

instance ToQuery StopStack

instance ToHeaders StopStack

instance ToJSON StopStack

data StopStackResponse = StopStackResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StopStackResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkStopStackResponse :: StopStackResponse
mkStopStackResponse = StopStackResponse

instance AWSRequest StopStack where
    type Sv StopStack = OpsWorks
    type Rs StopStack = StopStackResponse

    request = get
    response _ = nullaryResponse StopStackResponse
