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

-- Module      : Network.AWS.CloudFormation.DeleteStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified stack. Once the call completes successfully, stack
-- deletion starts. Deleted stacks do not show up in the 'DescribeStacks' API if
-- the deletion has been completed successfully.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeleteStack.html>
module Network.AWS.CloudFormation.DeleteStack
    (
    -- * Request
      DeleteStack
    -- ** Request constructor
    , deleteStack
    -- ** Request lenses
    , dsStackName

    -- * Response
    , DeleteStackResponse
    -- ** Response constructor
    , deleteStackResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

newtype DeleteStack = DeleteStack
    { _dsStackName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteStack' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsStackName' @::@ 'Text'
--
deleteStack :: Text -- ^ 'dsStackName'
            -> DeleteStack
deleteStack p1 = DeleteStack
    { _dsStackName = p1
    }

-- | The name or the unique identifier associated with the stack.
--
dsStackName :: Lens' DeleteStack Text
dsStackName = lens _dsStackName (\s a -> s { _dsStackName = a })

data DeleteStackResponse = DeleteStackResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteStackResponse' constructor.
deleteStackResponse :: DeleteStackResponse
deleteStackResponse = DeleteStackResponse

instance ToPath DeleteStack where
    toPath = const "/"

instance ToQuery DeleteStack where
    toQuery DeleteStack{..} = mconcat
        [ "StackName" =? _dsStackName
        ]

instance ToHeaders DeleteStack

instance AWSRequest DeleteStack where
    type Sv DeleteStack = CloudFormation
    type Rs DeleteStack = DeleteStackResponse

    request  = post "DeleteStack"
    response = nullResponse DeleteStackResponse
