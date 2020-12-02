{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetOriginRequestPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an origin request policy, including the following metadata:
--
--
--     * The policy’s identifier.
--
--     * The date and time when the policy was last modified.
--
--
--
-- To get an origin request policy, you must provide the policy’s identifier. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
module Network.AWS.CloudFront.GetOriginRequestPolicy
  ( -- * Creating a Request
    getOriginRequestPolicy,
    GetOriginRequestPolicy,

    -- * Request Lenses
    gorpId,

    -- * Destructuring the Response
    getOriginRequestPolicyResponse,
    GetOriginRequestPolicyResponse,

    -- * Response Lenses
    gorprsETag,
    gorprsOriginRequestPolicy,
    gorprsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getOriginRequestPolicy' smart constructor.
newtype GetOriginRequestPolicy = GetOriginRequestPolicy'
  { _gorpId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetOriginRequestPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gorpId' - The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
getOriginRequestPolicy ::
  -- | 'gorpId'
  Text ->
  GetOriginRequestPolicy
getOriginRequestPolicy pId_ =
  GetOriginRequestPolicy' {_gorpId = pId_}

-- | The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
gorpId :: Lens' GetOriginRequestPolicy Text
gorpId = lens _gorpId (\s a -> s {_gorpId = a})

instance AWSRequest GetOriginRequestPolicy where
  type Rs GetOriginRequestPolicy = GetOriginRequestPolicyResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          GetOriginRequestPolicyResponse'
            <$> (h .#? "ETag") <*> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetOriginRequestPolicy

instance NFData GetOriginRequestPolicy

instance ToHeaders GetOriginRequestPolicy where
  toHeaders = const mempty

instance ToPath GetOriginRequestPolicy where
  toPath GetOriginRequestPolicy' {..} =
    mconcat ["/2020-05-31/origin-request-policy/", toBS _gorpId]

instance ToQuery GetOriginRequestPolicy where
  toQuery = const mempty

-- | /See:/ 'getOriginRequestPolicyResponse' smart constructor.
data GetOriginRequestPolicyResponse = GetOriginRequestPolicyResponse'
  { _gorprsETag ::
      !(Maybe Text),
    _gorprsOriginRequestPolicy ::
      !(Maybe OriginRequestPolicy),
    _gorprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetOriginRequestPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gorprsETag' - The current version of the origin request policy.
--
-- * 'gorprsOriginRequestPolicy' - The origin request policy.
--
-- * 'gorprsResponseStatus' - -- | The response status code.
getOriginRequestPolicyResponse ::
  -- | 'gorprsResponseStatus'
  Int ->
  GetOriginRequestPolicyResponse
getOriginRequestPolicyResponse pResponseStatus_ =
  GetOriginRequestPolicyResponse'
    { _gorprsETag = Nothing,
      _gorprsOriginRequestPolicy = Nothing,
      _gorprsResponseStatus = pResponseStatus_
    }

-- | The current version of the origin request policy.
gorprsETag :: Lens' GetOriginRequestPolicyResponse (Maybe Text)
gorprsETag = lens _gorprsETag (\s a -> s {_gorprsETag = a})

-- | The origin request policy.
gorprsOriginRequestPolicy :: Lens' GetOriginRequestPolicyResponse (Maybe OriginRequestPolicy)
gorprsOriginRequestPolicy = lens _gorprsOriginRequestPolicy (\s a -> s {_gorprsOriginRequestPolicy = a})

-- | -- | The response status code.
gorprsResponseStatus :: Lens' GetOriginRequestPolicyResponse Int
gorprsResponseStatus = lens _gorprsResponseStatus (\s a -> s {_gorprsResponseStatus = a})

instance NFData GetOriginRequestPolicyResponse
