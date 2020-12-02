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
-- Module      : Network.AWS.Route53.GetTrafficPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specific traffic policy version.
--
--
module Network.AWS.Route53.GetTrafficPolicy
    (
    -- * Creating a Request
      getTrafficPolicy
    , GetTrafficPolicy
    -- * Request Lenses
    , gtpId
    , gtpVersion

    -- * Destructuring the Response
    , getTrafficPolicyResponse
    , GetTrafficPolicyResponse
    -- * Response Lenses
    , gtprsResponseStatus
    , gtprsTrafficPolicy
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | Gets information about a specific traffic policy version.
--
--
--
-- /See:/ 'getTrafficPolicy' smart constructor.
data GetTrafficPolicy = GetTrafficPolicy'
  { _gtpId      :: !Text
  , _gtpVersion :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTrafficPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtpId' - The ID of the traffic policy that you want to get information about.
--
-- * 'gtpVersion' - The version number of the traffic policy that you want to get information about.
getTrafficPolicy
    :: Text -- ^ 'gtpId'
    -> Natural -- ^ 'gtpVersion'
    -> GetTrafficPolicy
getTrafficPolicy pId_ pVersion_ =
  GetTrafficPolicy' {_gtpId = pId_, _gtpVersion = _Nat # pVersion_}


-- | The ID of the traffic policy that you want to get information about.
gtpId :: Lens' GetTrafficPolicy Text
gtpId = lens _gtpId (\ s a -> s{_gtpId = a})

-- | The version number of the traffic policy that you want to get information about.
gtpVersion :: Lens' GetTrafficPolicy Natural
gtpVersion = lens _gtpVersion (\ s a -> s{_gtpVersion = a}) . _Nat

instance AWSRequest GetTrafficPolicy where
        type Rs GetTrafficPolicy = GetTrafficPolicyResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetTrafficPolicyResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "TrafficPolicy"))

instance Hashable GetTrafficPolicy where

instance NFData GetTrafficPolicy where

instance ToHeaders GetTrafficPolicy where
        toHeaders = const mempty

instance ToPath GetTrafficPolicy where
        toPath GetTrafficPolicy'{..}
          = mconcat
              ["/2013-04-01/trafficpolicy/", toBS _gtpId, "/",
               toBS _gtpVersion]

instance ToQuery GetTrafficPolicy where
        toQuery = const mempty

-- | A complex type that contains the response information for the request.
--
--
--
-- /See:/ 'getTrafficPolicyResponse' smart constructor.
data GetTrafficPolicyResponse = GetTrafficPolicyResponse'
  { _gtprsResponseStatus :: !Int
  , _gtprsTrafficPolicy  :: !TrafficPolicy
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTrafficPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtprsResponseStatus' - -- | The response status code.
--
-- * 'gtprsTrafficPolicy' - A complex type that contains settings for the specified traffic policy.
getTrafficPolicyResponse
    :: Int -- ^ 'gtprsResponseStatus'
    -> TrafficPolicy -- ^ 'gtprsTrafficPolicy'
    -> GetTrafficPolicyResponse
getTrafficPolicyResponse pResponseStatus_ pTrafficPolicy_ =
  GetTrafficPolicyResponse'
    { _gtprsResponseStatus = pResponseStatus_
    , _gtprsTrafficPolicy = pTrafficPolicy_
    }


-- | -- | The response status code.
gtprsResponseStatus :: Lens' GetTrafficPolicyResponse Int
gtprsResponseStatus = lens _gtprsResponseStatus (\ s a -> s{_gtprsResponseStatus = a})

-- | A complex type that contains settings for the specified traffic policy.
gtprsTrafficPolicy :: Lens' GetTrafficPolicyResponse TrafficPolicy
gtprsTrafficPolicy = lens _gtprsTrafficPolicy (\ s a -> s{_gtprsTrafficPolicy = a})

instance NFData GetTrafficPolicyResponse where
