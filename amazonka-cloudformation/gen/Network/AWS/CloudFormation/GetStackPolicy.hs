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
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_GetStackPolicy.html>
module Network.AWS.CloudFormation.GetStackPolicy
    (
    -- * Request
      GetStackPolicy
    -- ** Request constructor
    , getStackPolicy
    -- ** Request lenses
    , gspStackName

    -- * Response
    , GetStackPolicyResponse
    -- ** Response constructor
    , getStackPolicyResponse
    -- ** Response lenses
    , gsprStackPolicyBody
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudFormation.Types
import qualified GHC.Exts

newtype GetStackPolicy = GetStackPolicy
    { _gspStackName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetStackPolicy' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gspStackName' @::@ 'Text'
--
getStackPolicy :: Text -- ^ 'gspStackName'
               -> GetStackPolicy
getStackPolicy p1 = GetStackPolicy
    { _gspStackName = p1
    }

-- | The name or stack ID that is associated with the stack whose policy you
-- want to get.
gspStackName :: Lens' GetStackPolicy Text
gspStackName = lens _gspStackName (\s a -> s { _gspStackName = a })

newtype GetStackPolicyResponse = GetStackPolicyResponse
    { _gsprStackPolicyBody :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'GetStackPolicyResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsprStackPolicyBody' @::@ 'Maybe' 'Text'
--
getStackPolicyResponse :: GetStackPolicyResponse
getStackPolicyResponse = GetStackPolicyResponse
    { _gsprStackPolicyBody = Nothing
    }

-- | Structure containing the stack policy body. (For more information, go to
-- Prevent Updates to Stack Resources in the AWS CloudFormation User
-- Guide.).
gsprStackPolicyBody :: Lens' GetStackPolicyResponse (Maybe Text)
gsprStackPolicyBody =
    lens _gsprStackPolicyBody (\s a -> s { _gsprStackPolicyBody = a })

instance ToPath GetStackPolicy where
    toPath = const "/"

instance ToQuery GetStackPolicy where
    toQuery GetStackPolicy{..} = mconcat
        [ "StackName" =? _gspStackName
        ]

instance ToHeaders GetStackPolicy

query

instance AWSRequest GetStackPolicy where
    type Sv GetStackPolicy = CloudFormation
    type Rs GetStackPolicy = GetStackPolicyResponse

    request  = post "GetStackPolicy"
    response = xmlResponse

instance FromXML GetStackPolicyResponse where
    parseXML = withElement "GetStackPolicyResult" $ \x -> GetStackPolicyResponse
        <$> x .@? "StackPolicyBody"
