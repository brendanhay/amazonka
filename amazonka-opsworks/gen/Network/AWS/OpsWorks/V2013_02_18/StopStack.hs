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
    , mkStopStackRequest
    -- ** Request lenses
    , ssssssssssssssssssssstStackId

    -- * Response
    , StopStackResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StopStack' request.
mkStopStackRequest :: Text -- ^ 'ssssssssssssssssssssstStackId'
                   -> StopStack
mkStopStackRequest p1 = StopStack
    { _ssssssssssssssssssssstStackId = p1
    }
{-# INLINE mkStopStackRequest #-}

newtype StopStack = StopStack
    { _ssssssssssssssssssssstStackId :: Text
      -- ^ The stack ID.
    } deriving (Show, Generic)

-- | The stack ID.
ssssssssssssssssssssstStackId :: Lens' StopStack (Text)
ssssssssssssssssssssstStackId = lens _ssssssssssssssssssssstStackId (\s a -> s { _ssssssssssssssssssssstStackId = a })
{-# INLINE ssssssssssssssssssssstStackId #-}

instance ToPath StopStack

instance ToQuery StopStack

instance ToHeaders StopStack

instance ToJSON StopStack

data StopStackResponse = StopStackResponse
    deriving (Eq, Show, Generic)

instance AWSRequest StopStack where
    type Sv StopStack = OpsWorks
    type Rs StopStack = StopStackResponse

    request = get
    response _ = nullaryResponse StopStackResponse
