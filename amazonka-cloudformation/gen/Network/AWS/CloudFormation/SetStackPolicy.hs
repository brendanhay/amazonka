{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.SetStackPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Sets a stack policy for a specified stack.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_SetStackPolicy.html>
module Network.AWS.CloudFormation.SetStackPolicy
    (
    -- * Request
      SetStackPolicy
    -- ** Request constructor
    , setStackPolicy
    -- ** Request lenses
    , ssprqStackPolicyBody
    , ssprqStackPolicyURL
    , ssprqStackName

    -- * Response
    , SetStackPolicyResponse
    -- ** Response constructor
    , setStackPolicyResponse
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the SetStackPolicy action.
--
-- /See:/ 'setStackPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssprqStackPolicyBody'
--
-- * 'ssprqStackPolicyURL'
--
-- * 'ssprqStackName'
data SetStackPolicy = SetStackPolicy'
    { _ssprqStackPolicyBody :: !(Maybe Text)
    , _ssprqStackPolicyURL  :: !(Maybe Text)
    , _ssprqStackName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetStackPolicy' smart constructor.
setStackPolicy :: Text -> SetStackPolicy
setStackPolicy pStackName =
    SetStackPolicy'
    { _ssprqStackPolicyBody = Nothing
    , _ssprqStackPolicyURL = Nothing
    , _ssprqStackName = pStackName
    }

-- | Structure containing the stack policy body. For more information, go to
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources>
-- in the AWS CloudFormation User Guide. You can specify either the
-- @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
ssprqStackPolicyBody :: Lens' SetStackPolicy (Maybe Text)
ssprqStackPolicyBody = lens _ssprqStackPolicyBody (\ s a -> s{_ssprqStackPolicyBody = a});

-- | Location of a file containing the stack policy. The URL must point to a
-- policy (max size: 16KB) located in an S3 bucket in the same region as
-- the stack. You can specify either the @StackPolicyBody@ or the
-- @StackPolicyURL@ parameter, but not both.
ssprqStackPolicyURL :: Lens' SetStackPolicy (Maybe Text)
ssprqStackPolicyURL = lens _ssprqStackPolicyURL (\ s a -> s{_ssprqStackPolicyURL = a});

-- | The name or unique stack ID that you want to associate a policy with.
ssprqStackName :: Lens' SetStackPolicy Text
ssprqStackName = lens _ssprqStackName (\ s a -> s{_ssprqStackName = a});

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
               "StackPolicyBody" =: _ssprqStackPolicyBody,
               "StackPolicyURL" =: _ssprqStackPolicyURL,
               "StackName" =: _ssprqStackName]

-- | /See:/ 'setStackPolicyResponse' smart constructor.
data SetStackPolicyResponse =
    SetStackPolicyResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetStackPolicyResponse' smart constructor.
setStackPolicyResponse :: SetStackPolicyResponse
setStackPolicyResponse = SetStackPolicyResponse'
