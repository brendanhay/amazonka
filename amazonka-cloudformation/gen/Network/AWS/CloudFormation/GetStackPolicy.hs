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
-- Module      : Network.AWS.CloudFormation.GetStackPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the stack policy for a specified stack. If a stack doesn't have a policy, a null value is returned.
--
--
module Network.AWS.CloudFormation.GetStackPolicy
    (
    -- * Creating a Request
      getStackPolicy
    , GetStackPolicy
    -- * Request Lenses
    , gspStackName

    -- * Destructuring the Response
    , getStackPolicyResponse
    , GetStackPolicyResponse
    -- * Response Lenses
    , gsprsStackPolicyBody
    , gsprsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'GetStackPolicy' action.
--
--
--
-- /See:/ 'getStackPolicy' smart constructor.
newtype GetStackPolicy = GetStackPolicy'
  { _gspStackName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStackPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gspStackName' - The name or unique stack ID that is associated with the stack whose policy you want to get.
getStackPolicy
    :: Text -- ^ 'gspStackName'
    -> GetStackPolicy
getStackPolicy pStackName_ = GetStackPolicy' {_gspStackName = pStackName_}


-- | The name or unique stack ID that is associated with the stack whose policy you want to get.
gspStackName :: Lens' GetStackPolicy Text
gspStackName = lens _gspStackName (\ s a -> s{_gspStackName = a})

instance AWSRequest GetStackPolicy where
        type Rs GetStackPolicy = GetStackPolicyResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "GetStackPolicyResult"
              (\ s h x ->
                 GetStackPolicyResponse' <$>
                   (x .@? "StackPolicyBody") <*> (pure (fromEnum s)))

instance Hashable GetStackPolicy where

instance NFData GetStackPolicy where

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

-- | The output for the 'GetStackPolicy' action.
--
--
--
-- /See:/ 'getStackPolicyResponse' smart constructor.
data GetStackPolicyResponse = GetStackPolicyResponse'
  { _gsprsStackPolicyBody :: !(Maybe Text)
  , _gsprsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetStackPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsprsStackPolicyBody' - Structure containing the stack policy body. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide.)
--
-- * 'gsprsResponseStatus' - -- | The response status code.
getStackPolicyResponse
    :: Int -- ^ 'gsprsResponseStatus'
    -> GetStackPolicyResponse
getStackPolicyResponse pResponseStatus_ =
  GetStackPolicyResponse'
    {_gsprsStackPolicyBody = Nothing, _gsprsResponseStatus = pResponseStatus_}


-- | Structure containing the stack policy body. (For more information, go to <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/protect-stack-resources.html Prevent Updates to Stack Resources> in the AWS CloudFormation User Guide.)
gsprsStackPolicyBody :: Lens' GetStackPolicyResponse (Maybe Text)
gsprsStackPolicyBody = lens _gsprsStackPolicyBody (\ s a -> s{_gsprsStackPolicyBody = a})

-- | -- | The response status code.
gsprsResponseStatus :: Lens' GetStackPolicyResponse Int
gsprsResponseStatus = lens _gsprsResponseStatus (\ s a -> s{_gsprsResponseStatus = a})

instance NFData GetStackPolicyResponse where
