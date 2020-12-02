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
-- Module      : Network.AWS.Route53.GetAccountLimit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified limit for the current account, for example, the maximum number of health checks that you can create using the account.
--
--
-- For the default limit, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ . To request a higher limit, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53 open a case> .
--
module Network.AWS.Route53.GetAccountLimit
    (
    -- * Creating a Request
      getAccountLimit
    , GetAccountLimit
    -- * Request Lenses
    , galType

    -- * Destructuring the Response
    , getAccountLimitResponse
    , GetAccountLimitResponse
    -- * Response Lenses
    , galrsResponseStatus
    , galrsLimit
    , galrsCount
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the request to create a hosted zone.
--
--
--
-- /See:/ 'getAccountLimit' smart constructor.
newtype GetAccountLimit = GetAccountLimit'
  { _galType :: AccountLimitType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'galType' - The limit that you want to get. Valid values include the following:     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
getAccountLimit
    :: AccountLimitType -- ^ 'galType'
    -> GetAccountLimit
getAccountLimit pType_ = GetAccountLimit' {_galType = pType_}


-- | The limit that you want to get. Valid values include the following:     * __MAX_HEALTH_CHECKS_BY_OWNER__ : The maximum number of health checks that you can create using the current account.     * __MAX_HOSTED_ZONES_BY_OWNER__ : The maximum number of hosted zones that you can create using the current account.     * __MAX_REUSABLE_DELEGATION_SETS_BY_OWNER__ : The maximum number of reusable delegation sets that you can create using the current account.     * __MAX_TRAFFIC_POLICIES_BY_OWNER__ : The maximum number of traffic policies that you can create using the current account.     * __MAX_TRAFFIC_POLICY_INSTANCES_BY_OWNER__ : The maximum number of traffic policy instances that you can create using the current account. (Traffic policy instances are referred to as traffic flow policy records in the Amazon Route 53 console.)
galType :: Lens' GetAccountLimit AccountLimitType
galType = lens _galType (\ s a -> s{_galType = a})

instance AWSRequest GetAccountLimit where
        type Rs GetAccountLimit = GetAccountLimitResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetAccountLimitResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Limit") <*>
                     (x .@ "Count"))

instance Hashable GetAccountLimit where

instance NFData GetAccountLimit where

instance ToHeaders GetAccountLimit where
        toHeaders = const mempty

instance ToPath GetAccountLimit where
        toPath GetAccountLimit'{..}
          = mconcat
              ["/2013-04-01/accountlimit/", toBS _galType]

instance ToQuery GetAccountLimit where
        toQuery = const mempty

-- | A complex type that contains the requested limit.
--
--
--
-- /See:/ 'getAccountLimitResponse' smart constructor.
data GetAccountLimitResponse = GetAccountLimitResponse'
  { _galrsResponseStatus :: !Int
  , _galrsLimit          :: !AccountLimit
  , _galrsCount          :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountLimitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'galrsResponseStatus' - -- | The response status code.
--
-- * 'galrsLimit' - The current setting for the specified limit. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of health checks that you can create using the current account.
--
-- * 'galrsCount' - The current number of entities that you have created of the specified type. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the request, the value of @Count@ is the current number of health checks that you have created using the current account.
getAccountLimitResponse
    :: Int -- ^ 'galrsResponseStatus'
    -> AccountLimit -- ^ 'galrsLimit'
    -> Natural -- ^ 'galrsCount'
    -> GetAccountLimitResponse
getAccountLimitResponse pResponseStatus_ pLimit_ pCount_ =
  GetAccountLimitResponse'
    { _galrsResponseStatus = pResponseStatus_
    , _galrsLimit = pLimit_
    , _galrsCount = _Nat # pCount_
    }


-- | -- | The response status code.
galrsResponseStatus :: Lens' GetAccountLimitResponse Int
galrsResponseStatus = lens _galrsResponseStatus (\ s a -> s{_galrsResponseStatus = a})

-- | The current setting for the specified limit. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of health checks that you can create using the current account.
galrsLimit :: Lens' GetAccountLimitResponse AccountLimit
galrsLimit = lens _galrsLimit (\ s a -> s{_galrsLimit = a})

-- | The current number of entities that you have created of the specified type. For example, if you specified @MAX_HEALTH_CHECKS_BY_OWNER@ for the value of @Type@ in the request, the value of @Count@ is the current number of health checks that you have created using the current account.
galrsCount :: Lens' GetAccountLimitResponse Natural
galrsCount = lens _galrsCount (\ s a -> s{_galrsCount = a}) . _Nat

instance NFData GetAccountLimitResponse where
