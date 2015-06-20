{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFormation.SetStackPolicy
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
    , sspStackPolicyBody
    , sspStackPolicyURL
    , sspStackName

    -- * Response
    , SetStackPolicyResponse
    -- ** Response constructor
    , setStackPolicyResponse
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setStackPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sspStackPolicyBody'
--
-- * 'sspStackPolicyURL'
--
-- * 'sspStackName'
data SetStackPolicy = SetStackPolicy'{_sspStackPolicyBody :: Maybe Text, _sspStackPolicyURL :: Maybe Text, _sspStackName :: Text} deriving (Eq, Read, Show)

-- | 'SetStackPolicy' smart constructor.
setStackPolicy :: Text -> SetStackPolicy
setStackPolicy pStackName = SetStackPolicy'{_sspStackPolicyBody = Nothing, _sspStackPolicyURL = Nothing, _sspStackName = pStackName};

-- | Structure containing the stack policy body. For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
sspStackPolicyBody :: Lens' SetStackPolicy (Maybe Text)
sspStackPolicyBody = lens _sspStackPolicyBody (\ s a -> s{_sspStackPolicyBody = a});

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as
-- the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
sspStackPolicyURL :: Lens' SetStackPolicy (Maybe Text)
sspStackPolicyURL = lens _sspStackPolicyURL (\ s a -> s{_sspStackPolicyURL = a});

-- | The name or unique stack ID that you want to associate a policy with.
sspStackName :: Lens' SetStackPolicy Text
sspStackName = lens _sspStackName (\ s a -> s{_sspStackName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest SetStackPolicy where
        type Sv SetStackPolicy = CloudFormation
        type Rs SetStackPolicy = SetStackPolicyResponse
        request = post
        response = receiveNull SetStackPolicyResponse'

instance ToHeaders SetStackPolicy where
        toHeaders = const mempty

instance ToPath SetStackPolicy where
        toPath = const "/"

instance ToQuery SetStackPolicy where
        toQuery SetStackPolicy'{..}
          = mconcat
              ["Action" =: ("SetStackPolicy" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackPolicyBody" =: _sspStackPolicyBody,
               "StackPolicyURL" =: _sspStackPolicyURL,
               "StackName" =: _sspStackName]

-- | /See:/ 'setStackPolicyResponse' smart constructor.
data SetStackPolicyResponse = SetStackPolicyResponse' deriving (Eq, Read, Show)

-- | 'SetStackPolicyResponse' smart constructor.
setStackPolicyResponse :: SetStackPolicyResponse
setStackPolicyResponse = SetStackPolicyResponse';
