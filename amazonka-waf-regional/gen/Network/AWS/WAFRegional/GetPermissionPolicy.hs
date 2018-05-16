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
-- Module      : Network.AWS.WAFRegional.GetPermissionPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the IAM policy attached to the RuleGroup.
--
--
module Network.AWS.WAFRegional.GetPermissionPolicy
    (
    -- * Creating a Request
      getPermissionPolicy
    , GetPermissionPolicy
    -- * Request Lenses
    , gppResourceARN

    -- * Destructuring the Response
    , getPermissionPolicyResponse
    , GetPermissionPolicyResponse
    -- * Response Lenses
    , gpprsPolicy
    , gpprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'getPermissionPolicy' smart constructor.
newtype GetPermissionPolicy = GetPermissionPolicy'
  { _gppResourceARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPermissionPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gppResourceARN' - The Amazon Resource Name (ARN) of the RuleGroup for which you want to get the policy.
getPermissionPolicy
    :: Text -- ^ 'gppResourceARN'
    -> GetPermissionPolicy
getPermissionPolicy pResourceARN_ =
  GetPermissionPolicy' {_gppResourceARN = pResourceARN_}


-- | The Amazon Resource Name (ARN) of the RuleGroup for which you want to get the policy.
gppResourceARN :: Lens' GetPermissionPolicy Text
gppResourceARN = lens _gppResourceARN (\ s a -> s{_gppResourceARN = a})

instance AWSRequest GetPermissionPolicy where
        type Rs GetPermissionPolicy =
             GetPermissionPolicyResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 GetPermissionPolicyResponse' <$>
                   (x .?> "Policy") <*> (pure (fromEnum s)))

instance Hashable GetPermissionPolicy where

instance NFData GetPermissionPolicy where

instance ToHeaders GetPermissionPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.GetPermissionPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPermissionPolicy where
        toJSON GetPermissionPolicy'{..}
          = object
              (catMaybes [Just ("ResourceArn" .= _gppResourceARN)])

instance ToPath GetPermissionPolicy where
        toPath = const "/"

instance ToQuery GetPermissionPolicy where
        toQuery = const mempty

-- | /See:/ 'getPermissionPolicyResponse' smart constructor.
data GetPermissionPolicyResponse = GetPermissionPolicyResponse'
  { _gpprsPolicy         :: !(Maybe Text)
  , _gpprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPermissionPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpprsPolicy' - The IAM policy attached to the specified RuleGroup.
--
-- * 'gpprsResponseStatus' - -- | The response status code.
getPermissionPolicyResponse
    :: Int -- ^ 'gpprsResponseStatus'
    -> GetPermissionPolicyResponse
getPermissionPolicyResponse pResponseStatus_ =
  GetPermissionPolicyResponse'
    {_gpprsPolicy = Nothing, _gpprsResponseStatus = pResponseStatus_}


-- | The IAM policy attached to the specified RuleGroup.
gpprsPolicy :: Lens' GetPermissionPolicyResponse (Maybe Text)
gpprsPolicy = lens _gpprsPolicy (\ s a -> s{_gpprsPolicy = a})

-- | -- | The response status code.
gpprsResponseStatus :: Lens' GetPermissionPolicyResponse Int
gpprsResponseStatus = lens _gpprsResponseStatus (\ s a -> s{_gpprsResponseStatus = a})

instance NFData GetPermissionPolicyResponse where
