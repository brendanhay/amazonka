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

-- Module      : Network.AWS.CloudFormation.SetStackPolicy
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

-- | Sets a stack policy for a specified stack.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_SetStackPolicy.html>
module Network.AWS.CloudFormation.SetStackPolicy
    (
    -- * Request
      SetStackPolicy
    -- ** Request constructor
    , setStackPolicy
    -- ** Request lenses
    , sspStackName
    , sspStackPolicyBody
    , sspStackPolicyURL

    -- * Response
    , SetStackPolicyResponse
    -- ** Response constructor
    , setStackPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

data SetStackPolicy = SetStackPolicy
    { _sspStackName       :: Text
    , _sspStackPolicyBody :: Maybe Text
    , _sspStackPolicyURL  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'SetStackPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sspStackName' @::@ 'Text'
--
-- * 'sspStackPolicyBody' @::@ 'Maybe' 'Text'
--
-- * 'sspStackPolicyURL' @::@ 'Maybe' 'Text'
--
setStackPolicy :: Text -- ^ 'sspStackName'
               -> SetStackPolicy
setStackPolicy p1 = SetStackPolicy
    { _sspStackName       = p1
    , _sspStackPolicyBody = Nothing
    , _sspStackPolicyURL  = Nothing
    }

-- | The name or unique stack ID that you want to associate a policy with.
sspStackName :: Lens' SetStackPolicy Text
sspStackName = lens _sspStackName (\s a -> s { _sspStackName = a })

-- | Structure containing the stack policy body. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide. You
-- can specify either the 'StackPolicyBody' or the 'StackPolicyURL' parameter, but
-- not both.
sspStackPolicyBody :: Lens' SetStackPolicy (Maybe Text)
sspStackPolicyBody =
    lens _sspStackPolicyBody (\s a -> s { _sspStackPolicyBody = a })

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as the
-- stack. You can specify either the 'StackPolicyBody' or the 'StackPolicyURL'
-- parameter, but not both.
sspStackPolicyURL :: Lens' SetStackPolicy (Maybe Text)
sspStackPolicyURL =
    lens _sspStackPolicyURL (\s a -> s { _sspStackPolicyURL = a })

data SetStackPolicyResponse = SetStackPolicyResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SetStackPolicyResponse' constructor.
setStackPolicyResponse :: SetStackPolicyResponse
setStackPolicyResponse = SetStackPolicyResponse

instance ToPath SetStackPolicy where
    toPath = const "/"

instance ToQuery SetStackPolicy where
    toQuery SetStackPolicy{..} = mconcat
        [ "StackName"       =? _sspStackName
        , "StackPolicyBody" =? _sspStackPolicyBody
        , "StackPolicyURL"  =? _sspStackPolicyURL
        ]

instance ToHeaders SetStackPolicy

instance AWSRequest SetStackPolicy where
    type Sv SetStackPolicy = CloudFormation
    type Rs SetStackPolicy = SetStackPolicyResponse

    request  = post "SetStackPolicy"
    response = nullResponse SetStackPolicyResponse
