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

-- Module      : Network.AWS.OpsWorks.DeleteStack
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

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.OpsWorks.Types

newtype DeleteStack = DeleteStack
    { _dsStackId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

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

instance ToPath DeleteStack where
    toPath = const "/"

instance ToQuery DeleteStack where
    toQuery = const mempty

instance ToHeaders DeleteStack

instance ToBody DeleteStack where
    toBody = toBody . encode . _dsStackId

data DeleteStackResponse = DeleteStackResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteStackResponse' constructor.
deleteStackResponse :: DeleteStackResponse
deleteStackResponse = DeleteStackResponse

instance AWSRequest DeleteStack where
    type Sv DeleteStack = OpsWorks
    type Rs DeleteStack = DeleteStackResponse

    request  = post
    response = nullaryResponse DeleteStackResponse
