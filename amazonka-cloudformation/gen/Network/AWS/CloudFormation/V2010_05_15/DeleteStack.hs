{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.DeleteStack
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a specified stack. Once the call completes successfully, stack
-- deletion starts. Deleted stacks do not show up in the DescribeStacks API if
-- the deletion has been completed successfully.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=DeleteStack
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
module Network.AWS.CloudFormation.V2010_05_15.DeleteStack
    (
    -- * Request
      DeleteStack
    -- ** Request constructor
    , deleteStack
    -- ** Request lenses
    , dsiStackName

    -- * Response
    , DeleteStackResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteStack' request.
deleteStack :: Text -- ^ 'dsiStackName'
            -> DeleteStack
deleteStack p1 = DeleteStack
    { _dsiStackName = p1
    }

data DeleteStack = DeleteStack
    { _dsiStackName :: Text
      -- ^ The name or the unique identifier associated with the stack.
    } deriving (Show, Generic)

-- | The name or the unique identifier associated with the stack.
dsiStackName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteStack
    -> f DeleteStack
dsiStackName f x =
    (\y -> x { _dsiStackName = y })
       <$> f (_dsiStackName x)
{-# INLINE dsiStackName #-}

instance ToQuery DeleteStack where
    toQuery = genericQuery def

data DeleteStackResponse = DeleteStackResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteStack where
    type Sv DeleteStack = CloudFormation
    type Rs DeleteStack = DeleteStackResponse

    request = post "DeleteStack"
    response _ = nullaryResponse DeleteStackResponse
