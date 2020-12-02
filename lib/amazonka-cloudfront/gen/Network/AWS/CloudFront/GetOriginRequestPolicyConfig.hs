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
-- Module      : Network.AWS.CloudFront.GetOriginRequestPolicyConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an origin request policy configuration.
--
--
-- To get an origin request policy configuration, you must provide the policy’s identifier. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
module Network.AWS.CloudFront.GetOriginRequestPolicyConfig
  ( -- * Creating a Request
    getOriginRequestPolicyConfig,
    GetOriginRequestPolicyConfig,

    -- * Request Lenses
    gorpcId,

    -- * Destructuring the Response
    getOriginRequestPolicyConfigResponse,
    GetOriginRequestPolicyConfigResponse,

    -- * Response Lenses
    gorpcrsETag,
    gorpcrsOriginRequestPolicyConfig,
    gorpcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getOriginRequestPolicyConfig' smart constructor.
newtype GetOriginRequestPolicyConfig = GetOriginRequestPolicyConfig'
  { _gorpcId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetOriginRequestPolicyConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gorpcId' - The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
getOriginRequestPolicyConfig ::
  -- | 'gorpcId'
  Text ->
  GetOriginRequestPolicyConfig
getOriginRequestPolicyConfig pId_ =
  GetOriginRequestPolicyConfig' {_gorpcId = pId_}

-- | The unique identifier for the origin request policy. If the origin request policy is attached to a distribution’s cache behavior, you can get the policy’s identifier using @ListDistributions@ or @GetDistribution@ . If the origin request policy is not attached to a cache behavior, you can get the identifier using @ListOriginRequestPolicies@ .
gorpcId :: Lens' GetOriginRequestPolicyConfig Text
gorpcId = lens _gorpcId (\s a -> s {_gorpcId = a})

instance AWSRequest GetOriginRequestPolicyConfig where
  type
    Rs GetOriginRequestPolicyConfig =
      GetOriginRequestPolicyConfigResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          GetOriginRequestPolicyConfigResponse'
            <$> (h .#? "ETag") <*> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetOriginRequestPolicyConfig

instance NFData GetOriginRequestPolicyConfig

instance ToHeaders GetOriginRequestPolicyConfig where
  toHeaders = const mempty

instance ToPath GetOriginRequestPolicyConfig where
  toPath GetOriginRequestPolicyConfig' {..} =
    mconcat
      ["/2020-05-31/origin-request-policy/", toBS _gorpcId, "/config"]

instance ToQuery GetOriginRequestPolicyConfig where
  toQuery = const mempty

-- | /See:/ 'getOriginRequestPolicyConfigResponse' smart constructor.
data GetOriginRequestPolicyConfigResponse = GetOriginRequestPolicyConfigResponse'
  { _gorpcrsETag ::
      !(Maybe Text),
    _gorpcrsOriginRequestPolicyConfig ::
      !( Maybe
           OriginRequestPolicyConfig
       ),
    _gorpcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetOriginRequestPolicyConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gorpcrsETag' - The current version of the origin request policy.
--
-- * 'gorpcrsOriginRequestPolicyConfig' - The origin request policy configuration.
--
-- * 'gorpcrsResponseStatus' - -- | The response status code.
getOriginRequestPolicyConfigResponse ::
  -- | 'gorpcrsResponseStatus'
  Int ->
  GetOriginRequestPolicyConfigResponse
getOriginRequestPolicyConfigResponse pResponseStatus_ =
  GetOriginRequestPolicyConfigResponse'
    { _gorpcrsETag = Nothing,
      _gorpcrsOriginRequestPolicyConfig = Nothing,
      _gorpcrsResponseStatus = pResponseStatus_
    }

-- | The current version of the origin request policy.
gorpcrsETag :: Lens' GetOriginRequestPolicyConfigResponse (Maybe Text)
gorpcrsETag = lens _gorpcrsETag (\s a -> s {_gorpcrsETag = a})

-- | The origin request policy configuration.
gorpcrsOriginRequestPolicyConfig :: Lens' GetOriginRequestPolicyConfigResponse (Maybe OriginRequestPolicyConfig)
gorpcrsOriginRequestPolicyConfig = lens _gorpcrsOriginRequestPolicyConfig (\s a -> s {_gorpcrsOriginRequestPolicyConfig = a})

-- | -- | The response status code.
gorpcrsResponseStatus :: Lens' GetOriginRequestPolicyConfigResponse Int
gorpcrsResponseStatus = lens _gorpcrsResponseStatus (\s a -> s {_gorpcrsResponseStatus = a})

instance NFData GetOriginRequestPolicyConfigResponse
