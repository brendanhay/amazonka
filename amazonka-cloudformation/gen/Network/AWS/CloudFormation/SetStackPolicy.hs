{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.SetStackPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a stack policy for a specified stack.
--
--
module Network.AWS.CloudFormation.SetStackPolicy
    (
    -- * Creating a Request
      setStackPolicy
    , SetStackPolicy
    -- * Request Lenses
    , sspStackPolicyBody
    , sspStackPolicyURL
    , sspStackName

    -- * Destructuring the Response
    , setStackPolicyResponse
    , SetStackPolicyResponse
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'SetStackPolicy' action.
--
--
--
-- /See:/ 'setStackPolicy' smart constructor.
data SetStackPolicy = SetStackPolicy'
  { _sspStackPolicyBody :: !(Maybe Text)
  , _sspStackPolicyURL  :: !(Maybe Text)
  , _sspStackName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetStackPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sspStackPolicyBody' - Structure containing the stack policy body. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- * 'sspStackPolicyURL' - Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
--
-- * 'sspStackName' - The name or unique stack ID that you want to associate a policy with.
setStackPolicy
    :: Text -- ^ 'sspStackName'
    -> SetStackPolicy
setStackPolicy pStackName_ =
  SetStackPolicy'
    { _sspStackPolicyBody = Nothing
    , _sspStackPolicyURL = Nothing
    , _sspStackName = pStackName_
    }


-- | Structure containing the stack policy body. For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
sspStackPolicyBody :: Lens' SetStackPolicy (Maybe Text)
sspStackPolicyBody = lens _sspStackPolicyBody (\ s a -> s{_sspStackPolicyBody = a})

-- | Location of a file containing the stack policy. The URL must point to a policy (maximum size: 16 KB) located in an S3 bucket in the same region as the stack. You can specify either the @StackPolicyBody@ or the @StackPolicyURL@ parameter, but not both.
sspStackPolicyURL :: Lens' SetStackPolicy (Maybe Text)
sspStackPolicyURL = lens _sspStackPolicyURL (\ s a -> s{_sspStackPolicyURL = a})

-- | The name or unique stack ID that you want to associate a policy with.
sspStackName :: Lens' SetStackPolicy Text
sspStackName = lens _sspStackName (\ s a -> s{_sspStackName = a})

instance AWSRequest SetStackPolicy where
        type Rs SetStackPolicy = SetStackPolicyResponse
        request = postQuery cloudFormation
        response = receiveNull SetStackPolicyResponse'

instance Hashable SetStackPolicy where

instance NFData SetStackPolicy where

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
data SetStackPolicyResponse =
  SetStackPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetStackPolicyResponse' with the minimum fields required to make a request.
--
setStackPolicyResponse
    :: SetStackPolicyResponse
setStackPolicyResponse = SetStackPolicyResponse'


instance NFData SetStackPolicyResponse where
