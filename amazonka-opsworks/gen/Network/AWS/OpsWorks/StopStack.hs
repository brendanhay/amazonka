{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.StopStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Stops a specified stack.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_StopStack.html>
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

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype StopStack = StopStack
    { _ss1StackId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

data StopStackResponse = StopStackResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'StopStackResponse' constructor.
stopStackResponse :: StopStackResponse
stopStackResponse = StopStackResponse

instance ToPath StopStack where
    toPath = const "/"

instance ToQuery StopStack where
    toQuery = const mempty

instance ToHeaders StopStack

instance ToJSON StopStack where
    toJSON StopStack{..} = object
        [ "StackId" .= _ss1StackId
        ]

instance AWSRequest StopStack where
    type Sv StopStack = OpsWorks
    type Rs StopStack = StopStackResponse

    request  = post "StopStack"
    response = nullResponse StopStackResponse
