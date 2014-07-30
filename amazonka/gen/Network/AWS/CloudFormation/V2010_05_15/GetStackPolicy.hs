{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.GetStackPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns the stack policy for a specified stack. If a stack doesn't have a
-- policy, a null value is returned.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=GetStackPolicy
-- &StackName=MyStack &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature] "{ "Statement" : [ { "Effect" : "Deny", "Action" :
-- "Update:*", "Principal" : "*", "Resource" :
-- "LogicalResourceId/ProductionDatabase" }, { "Effect" : "Allow", "Action" :
-- "Update:*", "Principal" : "*", "Resource" : "*" } ] }.
module Network.AWS.CloudFormation.V2010_05_15.GetStackPolicy where

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

data GetStackPolicy = GetStackPolicy
    { _gspiStackName :: Text
      -- ^ The name or stack ID that is associated with the stack whose
      -- policy you want to get.
    } deriving (Generic)

instance ToQuery GetStackPolicy where
    toQuery = genericToQuery def

instance AWSRequest GetStackPolicy where
    type Sv GetStackPolicy = CloudFormation
    type Rs GetStackPolicy = GetStackPolicyResponse

    request = post "GetStackPolicy"
    response _ = xmlResponse

data GetStackPolicyResponse = GetStackPolicyResponse
    { _gspoStackPolicyBody :: Maybe Text
      -- ^ Structure containing the stack policy body. (For more
      -- information, go to Prevent Updates to Stack Resources in the AWS
      -- CloudFormation User Guide.).
    } deriving (Generic)

instance FromXML GetStackPolicyResponse where
    fromXMLOptions = xmlOptions
