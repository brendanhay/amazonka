{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.StartStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts a stack's instances. Required Permissions: To use this action, an
-- IAM user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.StartStack
    (
    -- * Request
      StartStack
    -- ** Request constructor
    , startStack
    -- ** Request lenses
    , ssssssssssssssssssssxStackId

    -- * Response
    , StartStackResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'StartStack' request.
startStack :: Text -- ^ 'ssssssssssssssssssssxStackId'
           -> StartStack
startStack p1 = StartStack
    { _ssssssssssssssssssssxStackId = p1
    }
{-# INLINE startStack #-}

data StartStack = StartStack
    { _ssssssssssssssssssssxStackId :: Text
      -- ^ The stack ID.
    } deriving (Show, Generic)

-- | The stack ID.
ssssssssssssssssssssxStackId :: Lens' StartStack (Text)
ssssssssssssssssssssxStackId f x =
    f (_ssssssssssssssssssssxStackId x)
        <&> \y -> x { _ssssssssssssssssssssxStackId = y }
{-# INLINE ssssssssssssssssssssxStackId #-}

instance ToPath StartStack

instance ToQuery StartStack

instance ToHeaders StartStack

instance ToJSON StartStack

data StartStackResponse = StartStackResponse
    deriving (Eq, Show, Generic)

instance AWSRequest StartStack where
    type Sv StartStack = OpsWorks
    type Rs StartStack = StartStackResponse

    request = get
    response _ = nullaryResponse StartStackResponse
