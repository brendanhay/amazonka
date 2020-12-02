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
-- Module      : Network.AWS.FMS.GetPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager policy.
--
--
module Network.AWS.FMS.GetPolicy
    (
    -- * Creating a Request
      getPolicy
    , GetPolicy
    -- * Request Lenses
    , gpPolicyId

    -- * Destructuring the Response
    , getPolicyResponse
    , GetPolicyResponse
    -- * Response Lenses
    , gprsPolicyARN
    , gprsPolicy
    , gprsResponseStatus
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPolicy' smart constructor.
newtype GetPolicy = GetPolicy'
  { _gpPolicyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpPolicyId' - The ID of the AWS Firewall Manager policy that you want the details for.
getPolicy
    :: Text -- ^ 'gpPolicyId'
    -> GetPolicy
getPolicy pPolicyId_ = GetPolicy' {_gpPolicyId = pPolicyId_}


-- | The ID of the AWS Firewall Manager policy that you want the details for.
gpPolicyId :: Lens' GetPolicy Text
gpPolicyId = lens _gpPolicyId (\ s a -> s{_gpPolicyId = a})

instance AWSRequest GetPolicy where
        type Rs GetPolicy = GetPolicyResponse
        request = postJSON fms
        response
          = receiveJSON
              (\ s h x ->
                 GetPolicyResponse' <$>
                   (x .?> "PolicyArn") <*> (x .?> "Policy") <*>
                     (pure (fromEnum s)))

instance Hashable GetPolicy where

instance NFData GetPolicy where

instance ToHeaders GetPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.GetPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPolicy where
        toJSON GetPolicy'{..}
          = object
              (catMaybes [Just ("PolicyId" .= _gpPolicyId)])

instance ToPath GetPolicy where
        toPath = const "/"

instance ToQuery GetPolicy where
        toQuery = const mempty

-- | /See:/ 'getPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { _gprsPolicyARN      :: !(Maybe Text)
  , _gprsPolicy         :: !(Maybe Policy)
  , _gprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gprsPolicyARN' - The Amazon Resource Name (ARN) of the specified policy.
--
-- * 'gprsPolicy' - Information about the specified AWS Firewall Manager policy.
--
-- * 'gprsResponseStatus' - -- | The response status code.
getPolicyResponse
    :: Int -- ^ 'gprsResponseStatus'
    -> GetPolicyResponse
getPolicyResponse pResponseStatus_ =
  GetPolicyResponse'
    { _gprsPolicyARN = Nothing
    , _gprsPolicy = Nothing
    , _gprsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the specified policy.
gprsPolicyARN :: Lens' GetPolicyResponse (Maybe Text)
gprsPolicyARN = lens _gprsPolicyARN (\ s a -> s{_gprsPolicyARN = a})

-- | Information about the specified AWS Firewall Manager policy.
gprsPolicy :: Lens' GetPolicyResponse (Maybe Policy)
gprsPolicy = lens _gprsPolicy (\ s a -> s{_gprsPolicy = a})

-- | -- | The response status code.
gprsResponseStatus :: Lens' GetPolicyResponse Int
gprsResponseStatus = lens _gprsResponseStatus (\ s a -> s{_gprsResponseStatus = a})

instance NFData GetPolicyResponse where
