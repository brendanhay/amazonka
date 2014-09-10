{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudFormation
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
module Network.AWS.CloudFormation
    (
    -- * Request
      SetStackPolicy
    -- ** Request constructor
    , mkSetStackPolicy
    -- ** Request lenses
    , sspStackName
    , sspStackPolicyBody
    , sspStackPolicyURL

    -- * Response
    , SetStackPolicyResponse
    -- ** Response constructor
    , mkSetStackPolicyResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude

-- | The input for the SetStackPolicy action.
data SetStackPolicy = SetStackPolicy
    { _sspStackName :: !Text
    , _sspStackPolicyBody :: !(Maybe Text)
    , _sspStackPolicyURL :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetStackPolicy' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackName ::@ @Text@
--
-- * @StackPolicyBody ::@ @Maybe Text@
--
-- * @StackPolicyURL ::@ @Maybe Text@
--
mkSetStackPolicy :: Text -- ^ 'sspStackName'
                 -> SetStackPolicy
mkSetStackPolicy p1 = SetStackPolicy
    { _sspStackName = p1
    , _sspStackPolicyBody = Nothing
    , _sspStackPolicyURL = Nothing
    }

-- | The name or stack ID that you want to associate a policy with.
sspStackName :: Lens' SetStackPolicy Text
sspStackName = lens _sspStackName (\s a -> s { _sspStackName = a })

-- | Structure containing the stack policy body. For more information, go to
-- Prevent Updates to Stack Resources in the AWS CloudFormation User Guide.
-- You can specify either the StackPolicyBody or the StackPolicyURL parameter,
-- but not both.
sspStackPolicyBody :: Lens' SetStackPolicy (Maybe Text)
sspStackPolicyBody =
    lens _sspStackPolicyBody (\s a -> s { _sspStackPolicyBody = a })

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as the
-- stack. You can specify either the StackPolicyBody or the StackPolicyURL
-- parameter, but not both.
sspStackPolicyURL :: Lens' SetStackPolicy (Maybe Text)
sspStackPolicyURL =
    lens _sspStackPolicyURL (\s a -> s { _sspStackPolicyURL = a })

instance ToQuery SetStackPolicy where
    toQuery = genericQuery def

data SetStackPolicyResponse = SetStackPolicyResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetStackPolicyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSetStackPolicyResponse :: SetStackPolicyResponse
mkSetStackPolicyResponse = SetStackPolicyResponse

instance AWSRequest SetStackPolicy where
    type Sv SetStackPolicy = CloudFormation
    type Rs SetStackPolicy = SetStackPolicyResponse

    request = post "SetStackPolicy"
    response _ = nullaryResponse SetStackPolicyResponse
