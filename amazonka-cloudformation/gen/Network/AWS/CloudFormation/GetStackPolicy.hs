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

-- Module      : Network.AWS.CloudFormation.GetStackPolicy
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
module Network.AWS.CloudFormation.GetStackPolicy
    (
    -- * Request
      GetStackPolicyInput
    -- ** Request constructor
    , getStackPolicyInput
    -- ** Request lenses
    , gspiStackName

    -- * Response
    , GetStackPolicyOutput
    -- ** Response constructor
    , getStackPolicyOutput
    -- ** Response lenses
    , gspoStackPolicyBody
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types

newtype GetStackPolicyInput = GetStackPolicyInput
    { _gspiStackName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetStackPolicyInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gspiStackName' @::@ 'Text'
--
getStackPolicyInput :: Text -- ^ 'gspiStackName'
                    -> GetStackPolicyInput
getStackPolicyInput p1 = GetStackPolicyInput
    { _gspiStackName = p1
    }

-- | The name or stack ID that is associated with the stack whose policy you
-- want to get.
gspiStackName :: Lens' GetStackPolicyInput Text
gspiStackName = lens _gspiStackName (\s a -> s { _gspiStackName = a })

instance ToQuery GetStackPolicyInput

instance ToPath GetStackPolicyInput where
    toPath = const "/"

newtype GetStackPolicyOutput = GetStackPolicyOutput
    { _gspoStackPolicyBody :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetStackPolicyOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gspoStackPolicyBody' @::@ 'Maybe' 'Text'
--
getStackPolicyOutput :: GetStackPolicyOutput
getStackPolicyOutput = GetStackPolicyOutput
    { _gspoStackPolicyBody = Nothing
    }

-- | Structure containing the stack policy body. (For more information, go to
-- Prevent Updates to Stack Resources in the AWS CloudFormation User
-- Guide.).
gspoStackPolicyBody :: Lens' GetStackPolicyOutput (Maybe Text)
gspoStackPolicyBody =
    lens _gspoStackPolicyBody (\s a -> s { _gspoStackPolicyBody = a })

instance FromXML GetStackPolicyOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetStackPolicyOutput"

instance AWSRequest GetStackPolicyInput where
    type Sv GetStackPolicyInput = CloudFormation
    type Rs GetStackPolicyInput = GetStackPolicyOutput

    request  = post "GetStackPolicy"
    response = xmlResponse $ \h x -> GetStackPolicyOutput
        <$> x %| "StackPolicyBody"
