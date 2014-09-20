{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.StartStack
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
module Network.AWS.OpsWorks.StartStack
    (
    -- * Request
      StartStack
    -- ** Request constructor
    , startStack
    -- ** Request lenses
    , ss1StackId

    -- * Response
    , StartStackResponse
    -- ** Response constructor
    , startStackResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype StartStack = StartStack
    { _ss1StackId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartStack' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Text@
--
startStack :: Text -- ^ 'ss1StackId'
           -> StartStack
startStack p1 = StartStack
    { _ss1StackId = p1
    }

-- | The stack ID.
ss1StackId :: Lens' StartStack Text
ss1StackId = lens _ss1StackId (\s a -> s { _ss1StackId = a })

instance ToPath StartStack

instance ToQuery StartStack

instance ToHeaders StartStack

instance ToJSON StartStack

data StartStackResponse = StartStackResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartStackResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
startStackResponse :: StartStackResponse
startStackResponse = StartStackResponse

instance AWSRequest StartStack where
    type Sv StartStack = OpsWorks
    type Rs StartStack = StartStackResponse

    request = get
    response _ = nullaryResponse StartStackResponse
