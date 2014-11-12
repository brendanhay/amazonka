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

-- Module      : Network.AWS.CloudFormation.SetStackPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets a stack policy for a specified stack.
module Network.AWS.CloudFormation.SetStackPolicy
    (
    -- * Request
      SetStackPolicyInput
    -- ** Request constructor
    , setStackPolicyInput
    -- ** Request lenses
    , sspiStackName
    , sspiStackPolicyBody
    , sspiStackPolicyURL

    -- * Response
    , SetStackPolicyResponse
    -- ** Response constructor
    , setStackPolicyResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

data SetStackPolicyInput = SetStackPolicyInput
    { _sspiStackName       :: Text
    , _sspiStackPolicyBody :: Maybe Text
    , _sspiStackPolicyURL  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetStackPolicyInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sspiStackName' @::@ 'Text'
--
-- * 'sspiStackPolicyBody' @::@ 'Maybe' 'Text'
--
-- * 'sspiStackPolicyURL' @::@ 'Maybe' 'Text'
--
setStackPolicyInput :: Text -- ^ 'sspiStackName'
                    -> SetStackPolicyInput
setStackPolicyInput p1 = SetStackPolicyInput
    { _sspiStackName       = p1
    , _sspiStackPolicyBody = Nothing
    , _sspiStackPolicyURL  = Nothing
    }

-- | The name or stack ID that you want to associate a policy with.
sspiStackName :: Lens' SetStackPolicyInput Text
sspiStackName = lens _sspiStackName (\s a -> s { _sspiStackName = a })

-- | Structure containing the stack policy body. For more information, go to
-- Prevent Updates to Stack Resources in the AWS CloudFormation User Guide.
-- You can specify either the StackPolicyBody or the StackPolicyURL
-- parameter, but not both.
sspiStackPolicyBody :: Lens' SetStackPolicyInput (Maybe Text)
sspiStackPolicyBody =
    lens _sspiStackPolicyBody (\s a -> s { _sspiStackPolicyBody = a })

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as the
-- stack. You can specify either the StackPolicyBody or the StackPolicyURL
-- parameter, but not both.
sspiStackPolicyURL :: Lens' SetStackPolicyInput (Maybe Text)
sspiStackPolicyURL =
    lens _sspiStackPolicyURL (\s a -> s { _sspiStackPolicyURL = a })

instance ToQuery SetStackPolicyInput

instance ToPath SetStackPolicyInput where
    toPath = const "/"

data SetStackPolicyResponse = SetStackPolicyResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetStackPolicyResponse' constructor.
setStackPolicyResponse :: SetStackPolicyResponse
setStackPolicyResponse = SetStackPolicyResponse

instance FromXML SetStackPolicyResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SetStackPolicyResponse"

instance AWSRequest SetStackPolicyInput where
    type Sv SetStackPolicyInput = CloudFormation
    type Rs SetStackPolicyInput = SetStackPolicyResponse

    request  = post "SetStackPolicy"
    response = nullaryResponse SetStackPolicyResponse
