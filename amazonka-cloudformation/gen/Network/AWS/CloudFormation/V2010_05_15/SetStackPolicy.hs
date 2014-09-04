{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.CloudFormation.V2010_05_15.SetStackPolicy
    (
    -- * Request
      SetStackPolicy
    -- ** Request constructor
    , mkSetStackPolicyInput
    -- ** Request lenses
    , sspiStackName
    , sspiStackPolicyBody
    , sspiStackPolicyURL

    -- * Response
    , SetStackPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.V2010_05_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetStackPolicy' request.
mkSetStackPolicyInput :: Text -- ^ 'sspiStackName'
                      -> SetStackPolicy
mkSetStackPolicyInput p1 = SetStackPolicy
    { _sspiStackName = p1
    , _sspiStackPolicyBody = Nothing
    , _sspiStackPolicyURL = Nothing
    }
{-# INLINE mkSetStackPolicyInput #-}

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

-- | The name or stack ID that you want to associate a policy with.
sspiStackName :: Lens' SetStackPolicy (Text)
sspiStackName = lens _sspiStackName (\s a -> s { _sspiStackName = a })
{-# INLINE sspiStackName #-}

-- | Structure containing the stack policy body. For more information, go to
-- Prevent Updates to Stack Resources in the AWS CloudFormation User Guide.
-- You can specify either the StackPolicyBody or the StackPolicyURL parameter,
-- but not both.
sspiStackPolicyBody :: Lens' SetStackPolicy (Maybe Text)
sspiStackPolicyBody = lens _sspiStackPolicyBody (\s a -> s { _sspiStackPolicyBody = a })
{-# INLINE sspiStackPolicyBody #-}

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as the
-- stack. You can specify either the StackPolicyBody or the StackPolicyURL
-- parameter, but not both.
sspiStackPolicyURL :: Lens' SetStackPolicy (Maybe Text)
sspiStackPolicyURL = lens _sspiStackPolicyURL (\s a -> s { _sspiStackPolicyURL = a })
{-# INLINE sspiStackPolicyURL #-}

instance ToQuery SetStackPolicy where
    toQuery = genericQuery def

data SetStackPolicyResponse = SetStackPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetStackPolicy where
    type Sv SetStackPolicy = CloudFormation
    type Rs SetStackPolicy = SetStackPolicyResponse

    request = post "SetStackPolicy"
    response _ = nullaryResponse SetStackPolicyResponse
