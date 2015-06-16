{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFormation.GetStackPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the stack policy for a specified stack. If a stack doesn\'t have
-- a policy, a null value is returned.
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

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudFormation.Types

-- | /See:/ 'getStackPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gspStackName'
newtype GetStackPolicy = GetStackPolicy'{_gspStackName :: Text} deriving (Eq, Read, Show)

-- | 'GetStackPolicy' smart constructor.
getStackPolicy :: Text -> GetStackPolicy
getStackPolicy pStackName = GetStackPolicy'{_gspStackName = pStackName};

-- | The name or unique stack ID that is associated with the stack whose
-- policy you want to get.
gspStackName :: Lens' GetStackPolicy Text
gspStackName = lens _gspStackName (\ s a -> s{_gspStackName = a});

instance AWSRequest GetStackPolicy where
        type Sv GetStackPolicy = CloudFormation
        type Rs GetStackPolicy = GetStackPolicyResponse
        request = post
        response
          = receiveXMLWrapper "GetStackPolicyResult"
              (\ s h x ->
                 GetStackPolicyResponse' <$>
                   (x .@? "StackPolicyBody"))

instance ToHeaders GetStackPolicy where
        toHeaders = const mempty

instance ToPath GetStackPolicy where
        toPath = const "/"

instance ToQuery GetStackPolicy where
        toQuery GetStackPolicy'{..}
          = mconcat
              ["Action" =: ("GetStackPolicy" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _gspStackName]

-- | /See:/ 'getStackPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsprStackPolicyBody'
newtype GetStackPolicyResponse = GetStackPolicyResponse'{_gsprStackPolicyBody :: Maybe Text} deriving (Eq, Read, Show)

-- | 'GetStackPolicyResponse' smart constructor.
getStackPolicyResponse :: GetStackPolicyResponse
getStackPolicyResponse = GetStackPolicyResponse'{_gsprStackPolicyBody = Nothing};

-- | Structure containing the stack policy body. (For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide.)
gsprStackPolicyBody :: Lens' GetStackPolicyResponse (Maybe Text)
gsprStackPolicyBody = lens _gsprStackPolicyBody (\ s a -> s{_gsprStackPolicyBody = a});
