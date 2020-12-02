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
-- Module      : Network.AWS.CloudFront.GetMonitoringSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about whether additional CloudWatch metrics are enabled for the specified CloudFront distribution.
module Network.AWS.CloudFront.GetMonitoringSubscription
  ( -- * Creating a Request
    getMonitoringSubscription,
    GetMonitoringSubscription,

    -- * Request Lenses
    gmsDistributionId,

    -- * Destructuring the Response
    getMonitoringSubscriptionResponse,
    GetMonitoringSubscriptionResponse,

    -- * Response Lenses
    gmsrsMonitoringSubscription,
    gmsrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMonitoringSubscription' smart constructor.
newtype GetMonitoringSubscription = GetMonitoringSubscription'
  { _gmsDistributionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMonitoringSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsDistributionId' - The ID of the distribution that you are getting metrics information for.
getMonitoringSubscription ::
  -- | 'gmsDistributionId'
  Text ->
  GetMonitoringSubscription
getMonitoringSubscription pDistributionId_ =
  GetMonitoringSubscription' {_gmsDistributionId = pDistributionId_}

-- | The ID of the distribution that you are getting metrics information for.
gmsDistributionId :: Lens' GetMonitoringSubscription Text
gmsDistributionId = lens _gmsDistributionId (\s a -> s {_gmsDistributionId = a})

instance AWSRequest GetMonitoringSubscription where
  type
    Rs GetMonitoringSubscription =
      GetMonitoringSubscriptionResponse
  request = get cloudFront
  response =
    receiveXML
      ( \s h x ->
          GetMonitoringSubscriptionResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetMonitoringSubscription

instance NFData GetMonitoringSubscription

instance ToHeaders GetMonitoringSubscription where
  toHeaders = const mempty

instance ToPath GetMonitoringSubscription where
  toPath GetMonitoringSubscription' {..} =
    mconcat
      [ "/2020-05-31/distributions/",
        toBS _gmsDistributionId,
        "/monitoring-subscription"
      ]

instance ToQuery GetMonitoringSubscription where
  toQuery = const mempty

-- | /See:/ 'getMonitoringSubscriptionResponse' smart constructor.
data GetMonitoringSubscriptionResponse = GetMonitoringSubscriptionResponse'
  { _gmsrsMonitoringSubscription ::
      !( Maybe
           MonitoringSubscription
       ),
    _gmsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMonitoringSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsrsMonitoringSubscription' - A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- * 'gmsrsResponseStatus' - -- | The response status code.
getMonitoringSubscriptionResponse ::
  -- | 'gmsrsResponseStatus'
  Int ->
  GetMonitoringSubscriptionResponse
getMonitoringSubscriptionResponse pResponseStatus_ =
  GetMonitoringSubscriptionResponse'
    { _gmsrsMonitoringSubscription =
        Nothing,
      _gmsrsResponseStatus = pResponseStatus_
    }

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
gmsrsMonitoringSubscription :: Lens' GetMonitoringSubscriptionResponse (Maybe MonitoringSubscription)
gmsrsMonitoringSubscription = lens _gmsrsMonitoringSubscription (\s a -> s {_gmsrsMonitoringSubscription = a})

-- | -- | The response status code.
gmsrsResponseStatus :: Lens' GetMonitoringSubscriptionResponse Int
gmsrsResponseStatus = lens _gmsrsResponseStatus (\s a -> s {_gmsrsResponseStatus = a})

instance NFData GetMonitoringSubscriptionResponse
