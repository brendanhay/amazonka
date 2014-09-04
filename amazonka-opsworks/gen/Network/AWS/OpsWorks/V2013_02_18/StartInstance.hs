{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.StartInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts a specified instance. For more information, see Starting, Stopping,
-- and Rebooting Instances. Required Permissions: To use this action, an IAM
-- user must have a Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.StartInstance
    (
    -- * Request
      StartInstance
    -- ** Request constructor
    , mkStartInstanceRequest
    -- ** Request lenses
    , sirInstanceId

    -- * Response
    , StartInstanceResponse
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartInstance' request.
mkStartInstanceRequest :: Text -- ^ 'sirInstanceId'
                       -> StartInstance
mkStartInstanceRequest p1 = StartInstance
    { _sirInstanceId = p1
    }
{-# INLINE mkStartInstanceRequest #-}

newtype StartInstance = StartInstance
    { _sirInstanceId :: Text
      -- ^ The instance ID.
    } deriving (Show, Generic)

-- | The instance ID.
sirInstanceId :: Lens' StartInstance (Text)
sirInstanceId = lens _sirInstanceId (\s a -> s { _sirInstanceId = a })
{-# INLINE sirInstanceId #-}

instance ToPath StartInstance

instance ToQuery StartInstance

instance ToHeaders StartInstance

instance ToJSON StartInstance

data StartInstanceResponse = StartInstanceResponse
    deriving (Eq, Show, Generic)

instance AWSRequest StartInstance where
    type Sv StartInstance = OpsWorks
    type Rs StartInstance = StartInstanceResponse

    request = get
    response _ = nullaryResponse StartInstanceResponse
