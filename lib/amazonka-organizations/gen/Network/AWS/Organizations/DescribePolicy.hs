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
-- Module      : Network.AWS.Organizations.DescribePolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a policy.
--
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.DescribePolicy
    (
    -- * Creating a Request
      describePolicy
    , DescribePolicy
    -- * Request Lenses
    , dpPolicyId

    -- * Destructuring the Response
    , describePolicyResponse
    , DescribePolicyResponse
    -- * Response Lenses
    , dprsPolicy
    , dprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePolicy' smart constructor.
newtype DescribePolicy = DescribePolicy'
  { _dpPolicyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpPolicyId' - The unique identifier (ID) of the policy that you want details about. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lower-case letters or digits.
describePolicy
    :: Text -- ^ 'dpPolicyId'
    -> DescribePolicy
describePolicy pPolicyId_ = DescribePolicy' {_dpPolicyId = pPolicyId_}


-- | The unique identifier (ID) of the policy that you want details about. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lower-case letters or digits.
dpPolicyId :: Lens' DescribePolicy Text
dpPolicyId = lens _dpPolicyId (\ s a -> s{_dpPolicyId = a})

instance AWSRequest DescribePolicy where
        type Rs DescribePolicy = DescribePolicyResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 DescribePolicyResponse' <$>
                   (x .?> "Policy") <*> (pure (fromEnum s)))

instance Hashable DescribePolicy where

instance NFData DescribePolicy where

instance ToHeaders DescribePolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DescribePolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePolicy where
        toJSON DescribePolicy'{..}
          = object
              (catMaybes [Just ("PolicyId" .= _dpPolicyId)])

instance ToPath DescribePolicy where
        toPath = const "/"

instance ToQuery DescribePolicy where
        toQuery = const mempty

-- | /See:/ 'describePolicyResponse' smart constructor.
data DescribePolicyResponse = DescribePolicyResponse'
  { _dprsPolicy         :: !(Maybe Policy)
  , _dprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsPolicy' - A structure that contains details about the specified policy.
--
-- * 'dprsResponseStatus' - -- | The response status code.
describePolicyResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DescribePolicyResponse
describePolicyResponse pResponseStatus_ =
  DescribePolicyResponse'
    {_dprsPolicy = Nothing, _dprsResponseStatus = pResponseStatus_}


-- | A structure that contains details about the specified policy.
dprsPolicy :: Lens' DescribePolicyResponse (Maybe Policy)
dprsPolicy = lens _dprsPolicy (\ s a -> s{_dprsPolicy = a})

-- | -- | The response status code.
dprsResponseStatus :: Lens' DescribePolicyResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DescribePolicyResponse where
