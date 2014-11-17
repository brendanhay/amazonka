{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- <StartStack.html>
module Network.AWS.OpsWorks.StartStack
    (
    -- * Request
      StartStack
    -- ** Request constructor
    , startStack
    -- ** Request lenses
    , ss2StackId

    -- * Response
    , StartStackResponse
    -- ** Response constructor
    , startStackResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype StartStack = StartStack
    { _ss2StackId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'StartStack' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ss2StackId' @::@ 'Text'
--
startStack :: Text -- ^ 'ss2StackId'
           -> StartStack
startStack p1 = StartStack
    { _ss2StackId = p1
    }

-- | The stack ID.
ss2StackId :: Lens' StartStack Text
ss2StackId = lens _ss2StackId (\s a -> s { _ss2StackId = a })

data StartStackResponse = StartStackResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'StartStackResponse' constructor.
startStackResponse :: StartStackResponse
startStackResponse = StartStackResponse

instance AWSRequest StartStack where
    type Sv StartStack = OpsWorks
    type Rs StartStack = StartStackResponse

    request  = post
    response = nullResponse StartStackResponse

instance ToPath StartStack where
    toPath = const "/"

instance ToHeaders StartStack

instance ToQuery StartStack where
    toQuery = const mempty

instance ToJSON StartStack where
    toJSON = genericToJSON jsonOptions
