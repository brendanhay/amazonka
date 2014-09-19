{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- deletion starts. Deleted stacks do not show up in the DescribeStacks API if
-- the deletion has been completed successfully.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=DeleteStack
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
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

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude

-- | The input for DeleteStack action.
newtype DeleteStack = DeleteStack
    { _dsStackName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteStack' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackName ::@ @Text@
--
deleteStack :: Text -- ^ 'dsStackName'
            -> DeleteStack
deleteStack p1 = DeleteStack
    { _dsStackName = p1
    }

-- | The name or the unique identifier associated with the stack.
dsStackName :: Lens' DeleteStack Text
dsStackName = lens _dsStackName (\s a -> s { _dsStackName = a })

instance ToQuery DeleteStack where
    toQuery = genericQuery def

data DeleteStackResponse = DeleteStackResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteStackResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteStackResponse :: DeleteStackResponse
deleteStackResponse = DeleteStackResponse

instance AWSRequest DeleteStack where
    type Sv DeleteStack = CloudFormation
    type Rs DeleteStack = DeleteStackResponse

    request = post "DeleteStack"
    response _ = nullaryResponse DeleteStackResponse
