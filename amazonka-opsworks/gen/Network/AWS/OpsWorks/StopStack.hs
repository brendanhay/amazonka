{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.OpsWorks.StopStack
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
module Network.AWS.OpsWorks.StopStack
    (
    -- * Request
      StopStack
    -- ** Request constructor
    , stopStack
    -- ** Request lenses
    , ss1StackId

    -- * Response
    , StopStackResponse
    -- ** Response constructor
    , stopStackResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype StopStack = StopStack
    { _ss1StackId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'StopStack' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ss1StackId' @::@ 'Text'
--
stopStack :: Text -- ^ 'ss1StackId'
          -> StopStack
stopStack p1 = StopStack
    { _ss1StackId = p1
    }

-- | The stack ID.
ss1StackId :: Lens' StopStack Text
ss1StackId = lens _ss1StackId (\s a -> s { _ss1StackId = a })

instance ToPath StopStack where
    toPath = const "/"

instance ToQuery StopStack where
    toQuery = const mempty

instance ToHeaders StopStack

instance ToBody StopStack where
    toBody = toBody . encode . _ss1StackId

data StopStackResponse = StopStackResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'StopStackResponse' constructor.
stopStackResponse :: StopStackResponse
stopStackResponse = StopStackResponse

instance AWSRequest StopStack where
    type Sv StopStack = OpsWorks
    type Rs StopStack = StopStackResponse

    request  = post
    response = nullaryResponse StopStackResponse
