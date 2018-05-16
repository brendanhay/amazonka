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
-- Module      : Network.AWS.Route53.GetReusableDelegationSetLimit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
--
-- For the default limit, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html Limits> in the /Amazon Route 53 Developer Guide/ . To request a higher limit, <https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53 open a case> .
--
module Network.AWS.Route53.GetReusableDelegationSetLimit
    (
    -- * Creating a Request
      getReusableDelegationSetLimit
    , GetReusableDelegationSetLimit
    -- * Request Lenses
    , grdslType
    , grdslDelegationSetId

    -- * Destructuring the Response
    , getReusableDelegationSetLimitResponse
    , GetReusableDelegationSetLimitResponse
    -- * Response Lenses
    , grdslrsResponseStatus
    , grdslrsLimit
    , grdslrsCount
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
-- /See:/ 'getReusableDelegationSetLimit' smart constructor.
data GetReusableDelegationSetLimit = GetReusableDelegationSetLimit'
  { _grdslType            :: !ReusableDelegationSetLimitType
  , _grdslDelegationSetId :: !ResourceId
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReusableDelegationSetLimit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdslType' - Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- * 'grdslDelegationSetId' - The ID of the delegation set that you want to get the limit for.
getReusableDelegationSetLimit
    :: ReusableDelegationSetLimitType -- ^ 'grdslType'
    -> ResourceId -- ^ 'grdslDelegationSetId'
    -> GetReusableDelegationSetLimit
getReusableDelegationSetLimit pType_ pDelegationSetId_ =
  GetReusableDelegationSetLimit'
    {_grdslType = pType_, _grdslDelegationSetId = pDelegationSetId_}


-- | Specify @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ to get the maximum number of hosted zones that you can associate with the specified reusable delegation set.
grdslType :: Lens' GetReusableDelegationSetLimit ReusableDelegationSetLimitType
grdslType = lens _grdslType (\ s a -> s{_grdslType = a})

-- | The ID of the delegation set that you want to get the limit for.
grdslDelegationSetId :: Lens' GetReusableDelegationSetLimit ResourceId
grdslDelegationSetId = lens _grdslDelegationSetId (\ s a -> s{_grdslDelegationSetId = a})

instance AWSRequest GetReusableDelegationSetLimit
         where
        type Rs GetReusableDelegationSetLimit =
             GetReusableDelegationSetLimitResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetReusableDelegationSetLimitResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Limit") <*>
                     (x .@ "Count"))

instance Hashable GetReusableDelegationSetLimit where

instance NFData GetReusableDelegationSetLimit where

instance ToHeaders GetReusableDelegationSetLimit
         where
        toHeaders = const mempty

instance ToPath GetReusableDelegationSetLimit where
        toPath GetReusableDelegationSetLimit'{..}
          = mconcat
              ["/2013-04-01/reusabledelegationsetlimit/",
               toBS _grdslDelegationSetId, "/", toBS _grdslType]

instance ToQuery GetReusableDelegationSetLimit where
        toQuery = const mempty

-- | A complex type that contains the requested limit.
--
--
--
-- /See:/ 'getReusableDelegationSetLimitResponse' smart constructor.
data GetReusableDelegationSetLimitResponse = GetReusableDelegationSetLimitResponse'
  { _grdslrsResponseStatus :: !Int
  , _grdslrsLimit          :: !ReusableDelegationSetLimit
  , _grdslrsCount          :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReusableDelegationSetLimitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdslrsResponseStatus' - -- | The response status code.
--
-- * 'grdslrsLimit' - The current setting for the limit on hosted zones that you can associate with the specified reusable delegation set.
--
-- * 'grdslrsCount' - The current number of hosted zones that you can associate with the specified reusable delegation set.
getReusableDelegationSetLimitResponse
    :: Int -- ^ 'grdslrsResponseStatus'
    -> ReusableDelegationSetLimit -- ^ 'grdslrsLimit'
    -> Natural -- ^ 'grdslrsCount'
    -> GetReusableDelegationSetLimitResponse
getReusableDelegationSetLimitResponse pResponseStatus_ pLimit_ pCount_ =
  GetReusableDelegationSetLimitResponse'
    { _grdslrsResponseStatus = pResponseStatus_
    , _grdslrsLimit = pLimit_
    , _grdslrsCount = _Nat # pCount_
    }


-- | -- | The response status code.
grdslrsResponseStatus :: Lens' GetReusableDelegationSetLimitResponse Int
grdslrsResponseStatus = lens _grdslrsResponseStatus (\ s a -> s{_grdslrsResponseStatus = a})

-- | The current setting for the limit on hosted zones that you can associate with the specified reusable delegation set.
grdslrsLimit :: Lens' GetReusableDelegationSetLimitResponse ReusableDelegationSetLimit
grdslrsLimit = lens _grdslrsLimit (\ s a -> s{_grdslrsLimit = a})

-- | The current number of hosted zones that you can associate with the specified reusable delegation set.
grdslrsCount :: Lens' GetReusableDelegationSetLimitResponse Natural
grdslrsCount = lens _grdslrsCount (\ s a -> s{_grdslrsCount = a}) . _Nat

instance NFData GetReusableDelegationSetLimitResponse
         where
