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
-- Module      : Network.AWS.Route53.GetTrafficPolicyInstanceCount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the number of traffic policy instances that are associated with the current AWS account.
--
--
module Network.AWS.Route53.GetTrafficPolicyInstanceCount
    (
    -- * Creating a Request
      getTrafficPolicyInstanceCount
    , GetTrafficPolicyInstanceCount

    -- * Destructuring the Response
    , getTrafficPolicyInstanceCountResponse
    , GetTrafficPolicyInstanceCountResponse
    -- * Response Lenses
    , gtpicrsResponseStatus
    , gtpicrsTrafficPolicyInstanceCount
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | Request to get the number of traffic policy instances that are associated with the current AWS account.
--
--
--
-- /See:/ 'getTrafficPolicyInstanceCount' smart constructor.
data GetTrafficPolicyInstanceCount =
  GetTrafficPolicyInstanceCount'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTrafficPolicyInstanceCount' with the minimum fields required to make a request.
--
getTrafficPolicyInstanceCount
    :: GetTrafficPolicyInstanceCount
getTrafficPolicyInstanceCount = GetTrafficPolicyInstanceCount'


instance AWSRequest GetTrafficPolicyInstanceCount
         where
        type Rs GetTrafficPolicyInstanceCount =
             GetTrafficPolicyInstanceCountResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetTrafficPolicyInstanceCountResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@ "TrafficPolicyInstanceCount"))

instance Hashable GetTrafficPolicyInstanceCount where

instance NFData GetTrafficPolicyInstanceCount where

instance ToHeaders GetTrafficPolicyInstanceCount
         where
        toHeaders = const mempty

instance ToPath GetTrafficPolicyInstanceCount where
        toPath
          = const "/2013-04-01/trafficpolicyinstancecount"

instance ToQuery GetTrafficPolicyInstanceCount where
        toQuery = const mempty

-- | A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.
--
--
--
-- /See:/ 'getTrafficPolicyInstanceCountResponse' smart constructor.
data GetTrafficPolicyInstanceCountResponse = GetTrafficPolicyInstanceCountResponse'
  { _gtpicrsResponseStatus             :: !Int
  , _gtpicrsTrafficPolicyInstanceCount :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTrafficPolicyInstanceCountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtpicrsResponseStatus' - -- | The response status code.
--
-- * 'gtpicrsTrafficPolicyInstanceCount' - The number of traffic policy instances that are associated with the current AWS account.
getTrafficPolicyInstanceCountResponse
    :: Int -- ^ 'gtpicrsResponseStatus'
    -> Int -- ^ 'gtpicrsTrafficPolicyInstanceCount'
    -> GetTrafficPolicyInstanceCountResponse
getTrafficPolicyInstanceCountResponse pResponseStatus_ pTrafficPolicyInstanceCount_ =
  GetTrafficPolicyInstanceCountResponse'
    { _gtpicrsResponseStatus = pResponseStatus_
    , _gtpicrsTrafficPolicyInstanceCount = pTrafficPolicyInstanceCount_
    }


-- | -- | The response status code.
gtpicrsResponseStatus :: Lens' GetTrafficPolicyInstanceCountResponse Int
gtpicrsResponseStatus = lens _gtpicrsResponseStatus (\ s a -> s{_gtpicrsResponseStatus = a})

-- | The number of traffic policy instances that are associated with the current AWS account.
gtpicrsTrafficPolicyInstanceCount :: Lens' GetTrafficPolicyInstanceCountResponse Int
gtpicrsTrafficPolicyInstanceCount = lens _gtpicrsTrafficPolicyInstanceCount (\ s a -> s{_gtpicrsTrafficPolicyInstanceCount = a})

instance NFData GetTrafficPolicyInstanceCountResponse
         where
