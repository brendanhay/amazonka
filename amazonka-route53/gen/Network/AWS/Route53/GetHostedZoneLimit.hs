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
-- Module      : Network.AWS.Route53.GetHostedZoneLimit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified limit for a specified hosted zone, for example, the maximum number of records that you can create in the hosted zone.
--
--
-- For the default limit, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ . To request a higher limit, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53 open a case> .
--
module Network.AWS.Route53.GetHostedZoneLimit
    (
    -- * Creating a Request
      getHostedZoneLimit
    , GetHostedZoneLimit
    -- * Request Lenses
    , ghzlType
    , ghzlHostedZoneId

    -- * Destructuring the Response
    , getHostedZoneLimitResponse
    , GetHostedZoneLimitResponse
    -- * Response Lenses
    , ghzlrsResponseStatus
    , ghzlrsLimit
    , ghzlrsCount
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
-- /See:/ 'getHostedZoneLimit' smart constructor.
data GetHostedZoneLimit = GetHostedZoneLimit'
  { _ghzlType         :: !HostedZoneLimitType
  , _ghzlHostedZoneId :: !ResourceId
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHostedZoneLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghzlType' - The limit that you want to get. Valid values include the following:     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
--
-- * 'ghzlHostedZoneId' - The ID of the hosted zone that you want to get a limit for.
getHostedZoneLimit
    :: HostedZoneLimitType -- ^ 'ghzlType'
    -> ResourceId -- ^ 'ghzlHostedZoneId'
    -> GetHostedZoneLimit
getHostedZoneLimit pType_ pHostedZoneId_ =
  GetHostedZoneLimit' {_ghzlType = pType_, _ghzlHostedZoneId = pHostedZoneId_}


-- | The limit that you want to get. Valid values include the following:     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
ghzlType :: Lens' GetHostedZoneLimit HostedZoneLimitType
ghzlType = lens _ghzlType (\ s a -> s{_ghzlType = a})

-- | The ID of the hosted zone that you want to get a limit for.
ghzlHostedZoneId :: Lens' GetHostedZoneLimit ResourceId
ghzlHostedZoneId = lens _ghzlHostedZoneId (\ s a -> s{_ghzlHostedZoneId = a})

instance AWSRequest GetHostedZoneLimit where
        type Rs GetHostedZoneLimit =
             GetHostedZoneLimitResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetHostedZoneLimitResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Limit") <*>
                     (x .@ "Count"))

instance Hashable GetHostedZoneLimit where

instance NFData GetHostedZoneLimit where

instance ToHeaders GetHostedZoneLimit where
        toHeaders = const mempty

instance ToPath GetHostedZoneLimit where
        toPath GetHostedZoneLimit'{..}
          = mconcat
              ["/2013-04-01/hostedzonelimit/",
               toBS _ghzlHostedZoneId, "/", toBS _ghzlType]

instance ToQuery GetHostedZoneLimit where
        toQuery = const mempty

-- | A complex type that contains the requested limit.
--
--
--
-- /See:/ 'getHostedZoneLimitResponse' smart constructor.
data GetHostedZoneLimitResponse = GetHostedZoneLimitResponse'
  { _ghzlrsResponseStatus :: !Int
  , _ghzlrsLimit          :: !HostedZoneLimit
  , _ghzlrsCount          :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHostedZoneLimitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghzlrsResponseStatus' - -- | The response status code.
--
-- * 'ghzlrsLimit' - The current setting for the specified limit. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of records that you can create in the specified hosted zone.
--
-- * 'ghzlrsCount' - The current number of entities that you have created of the specified type. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Count@ is the current number of records that you have created in the specified hosted zone.
getHostedZoneLimitResponse
    :: Int -- ^ 'ghzlrsResponseStatus'
    -> HostedZoneLimit -- ^ 'ghzlrsLimit'
    -> Natural -- ^ 'ghzlrsCount'
    -> GetHostedZoneLimitResponse
getHostedZoneLimitResponse pResponseStatus_ pLimit_ pCount_ =
  GetHostedZoneLimitResponse'
    { _ghzlrsResponseStatus = pResponseStatus_
    , _ghzlrsLimit = pLimit_
    , _ghzlrsCount = _Nat # pCount_
    }


-- | -- | The response status code.
ghzlrsResponseStatus :: Lens' GetHostedZoneLimitResponse Int
ghzlrsResponseStatus = lens _ghzlrsResponseStatus (\ s a -> s{_ghzlrsResponseStatus = a})

-- | The current setting for the specified limit. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Limit@ is the maximum number of records that you can create in the specified hosted zone.
ghzlrsLimit :: Lens' GetHostedZoneLimitResponse HostedZoneLimit
ghzlrsLimit = lens _ghzlrsLimit (\ s a -> s{_ghzlrsLimit = a})

-- | The current number of entities that you have created of the specified type. For example, if you specified @MAX_RRSETS_BY_ZONE@ for the value of @Type@ in the request, the value of @Count@ is the current number of records that you have created in the specified hosted zone.
ghzlrsCount :: Lens' GetHostedZoneLimitResponse Natural
ghzlrsCount = lens _ghzlrsCount (\ s a -> s{_ghzlrsCount = a}) . _Nat

instance NFData GetHostedZoneLimitResponse where
