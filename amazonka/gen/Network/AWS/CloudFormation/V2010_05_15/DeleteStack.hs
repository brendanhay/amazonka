{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.CloudFormation.V2010_05_15.DeleteStack where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.CloudFormation.V2010_05_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteStack = DeleteStack
    { _dsiStackName :: Text
      -- ^ The name or the unique identifier associated with the stack.
    } deriving (Generic)

instance ToQuery DeleteStack where
    toQuery = genericToQuery def

instance AWSRequest DeleteStack where
    type Sv DeleteStack = CloudFormation
    type Rs DeleteStack = DeleteStackResponse

    request = post "DeleteStack"
    response _ _ = return (Right DeleteStackResponse)

data DeleteStackResponse = DeleteStackResponse
    deriving (Eq, Show, Generic)
