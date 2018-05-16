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
-- Module      : Network.AWS.FMS.GetComplianceDetail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed compliance information about the specified member account. Details include resources that are in and out of compliance with the specified policy. Resources are considered non-compliant if the specified policy has not been applied to them.
--
--
module Network.AWS.FMS.GetComplianceDetail
    (
    -- * Creating a Request
      getComplianceDetail
    , GetComplianceDetail
    -- * Request Lenses
    , gcdPolicyId
    , gcdMemberAccount

    -- * Destructuring the Response
    , getComplianceDetailResponse
    , GetComplianceDetailResponse
    -- * Response Lenses
    , gcdrsPolicyComplianceDetail
    , gcdrsResponseStatus
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getComplianceDetail' smart constructor.
data GetComplianceDetail = GetComplianceDetail'
  { _gcdPolicyId      :: !Text
  , _gcdMemberAccount :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetComplianceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdPolicyId' - The ID of the policy that you want to get the details for. @PolicyId@ is returned by @PutPolicy@ and by @ListPolicies@ .
--
-- * 'gcdMemberAccount' - The AWS account that owns the resources that you want to get the details for.
getComplianceDetail
    :: Text -- ^ 'gcdPolicyId'
    -> Text -- ^ 'gcdMemberAccount'
    -> GetComplianceDetail
getComplianceDetail pPolicyId_ pMemberAccount_ =
  GetComplianceDetail'
    {_gcdPolicyId = pPolicyId_, _gcdMemberAccount = pMemberAccount_}


-- | The ID of the policy that you want to get the details for. @PolicyId@ is returned by @PutPolicy@ and by @ListPolicies@ .
gcdPolicyId :: Lens' GetComplianceDetail Text
gcdPolicyId = lens _gcdPolicyId (\ s a -> s{_gcdPolicyId = a})

-- | The AWS account that owns the resources that you want to get the details for.
gcdMemberAccount :: Lens' GetComplianceDetail Text
gcdMemberAccount = lens _gcdMemberAccount (\ s a -> s{_gcdMemberAccount = a})

instance AWSRequest GetComplianceDetail where
        type Rs GetComplianceDetail =
             GetComplianceDetailResponse
        request = postJSON fms
        response
          = receiveJSON
              (\ s h x ->
                 GetComplianceDetailResponse' <$>
                   (x .?> "PolicyComplianceDetail") <*>
                     (pure (fromEnum s)))

instance Hashable GetComplianceDetail where

instance NFData GetComplianceDetail where

instance ToHeaders GetComplianceDetail where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.GetComplianceDetail" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetComplianceDetail where
        toJSON GetComplianceDetail'{..}
          = object
              (catMaybes
                 [Just ("PolicyId" .= _gcdPolicyId),
                  Just ("MemberAccount" .= _gcdMemberAccount)])

instance ToPath GetComplianceDetail where
        toPath = const "/"

instance ToQuery GetComplianceDetail where
        toQuery = const mempty

-- | /See:/ 'getComplianceDetailResponse' smart constructor.
data GetComplianceDetailResponse = GetComplianceDetailResponse'
  { _gcdrsPolicyComplianceDetail :: !(Maybe PolicyComplianceDetail)
  , _gcdrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetComplianceDetailResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdrsPolicyComplianceDetail' - Information about the resources and the policy that you specified in the @GetComplianceDetail@ request.
--
-- * 'gcdrsResponseStatus' - -- | The response status code.
getComplianceDetailResponse
    :: Int -- ^ 'gcdrsResponseStatus'
    -> GetComplianceDetailResponse
getComplianceDetailResponse pResponseStatus_ =
  GetComplianceDetailResponse'
    { _gcdrsPolicyComplianceDetail = Nothing
    , _gcdrsResponseStatus = pResponseStatus_
    }


-- | Information about the resources and the policy that you specified in the @GetComplianceDetail@ request.
gcdrsPolicyComplianceDetail :: Lens' GetComplianceDetailResponse (Maybe PolicyComplianceDetail)
gcdrsPolicyComplianceDetail = lens _gcdrsPolicyComplianceDetail (\ s a -> s{_gcdrsPolicyComplianceDetail = a})

-- | -- | The response status code.
gcdrsResponseStatus :: Lens' GetComplianceDetailResponse Int
gcdrsResponseStatus = lens _gcdrsResponseStatus (\ s a -> s{_gcdrsResponseStatus = a})

instance NFData GetComplianceDetailResponse where
