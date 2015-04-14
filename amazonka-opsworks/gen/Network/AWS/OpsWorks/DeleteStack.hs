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

-- Module      : Network.AWS.OpsWorks.DeleteStack
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

-- | Deletes a specified stack. You must first delete all instances, layers, and
-- apps or deregister registered instances. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-shutting.html Shut Downa Stack>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_DeleteStack.html>
module Network.AWS.OpsWorks.DeleteStack
    (
    -- * Request
      DeleteStack
    -- ** Request constructor
    , deleteStack
    -- ** Request lenses
    , dsStackId

    -- * Response
    , DeleteStackResponse
    -- ** Response constructor
    , deleteStackResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

newtype DeleteStack = DeleteStack
    { _dsStackId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteStack' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsStackId' @::@ 'Text'
--
deleteStack :: Text -- ^ 'dsStackId'
            -> DeleteStack
deleteStack p1 = DeleteStack
    { _dsStackId = p1
    }

-- | The stack ID.
dsStackId :: Lens' DeleteStack Text
dsStackId = lens _dsStackId (\s a -> s { _dsStackId = a })

data DeleteStackResponse = DeleteStackResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteStackResponse' constructor.
deleteStackResponse :: DeleteStackResponse
deleteStackResponse = DeleteStackResponse

instance ToPath DeleteStack where
    toPath = const "/"

instance ToQuery DeleteStack where
    toQuery = const mempty

instance ToHeaders DeleteStack

instance ToJSON DeleteStack where
    toJSON DeleteStack{..} = object
        [ "StackId" .= _dsStackId
        ]

instance AWSRequest DeleteStack where
    type Sv DeleteStack = OpsWorks
    type Rs DeleteStack = DeleteStackResponse

    request  = post "DeleteStack"
    response = nullResponse DeleteStackResponse
