{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation.V2010_05_15.SetStackPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets a stack policy for a specified stack.
-- https://cloudformation.us-east-1.amazonaws.com/ ?Action=SetStackPolicy
-- &StackName=MyStack &StackPolicyBody=[Stack Policy Document]
-- &Version=2010-05-15 &SignatureVersion=2
-- &Timestamp=2010-07-27T22%3A26%3A28.000Z &AWSAccessKeyId=[AWS Access KeyID]
-- &Signature=[Signature].
module Network.AWS.CloudFormation.V2010_05_15.SetStackPolicy where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'SetStackPolicy' request.
setStackPolicy :: Text -- ^ '_sspiStackName'
               -> SetStackPolicy
setStackPolicy p1 = SetStackPolicy
    { _sspiStackName = p1
    , _sspiStackPolicyBody = Nothing
    , _sspiStackPolicyURL = Nothing
    }

data SetStackPolicy = SetStackPolicy
    { _sspiStackName :: Text
      -- ^ The name or stack ID that you want to associate a policy with.
    , _sspiStackPolicyBody :: Maybe Text
      -- ^ Structure containing the stack policy body. For more information,
      -- go to Prevent Updates to Stack Resources in the AWS
      -- CloudFormation User Guide. You can specify either the
      -- StackPolicyBody or the StackPolicyURL parameter, but not both.
    , _sspiStackPolicyURL :: Maybe Text
      -- ^ Location of a file containing the stack policy. The URL must
      -- point to a policy (max size: 16KB) located in an S3 bucket in the
      -- same region as the stack. You can specify either the
      -- StackPolicyBody or the StackPolicyURL parameter, but not both.
    } deriving (Show, Generic)

makeLenses ''SetStackPolicy

instance ToQuery SetStackPolicy where
    toQuery = genericQuery def

data SetStackPolicyResponse = SetStackPolicyResponse
    deriving (Eq, Show, Generic)

makeLenses ''SetStackPolicyResponse

instance AWSRequest SetStackPolicy where
    type Sv SetStackPolicy = CloudFormation
    type Rs SetStackPolicy = SetStackPolicyResponse

    request = post "SetStackPolicy"
    response _ = nullaryResponse SetStackPolicyResponse
