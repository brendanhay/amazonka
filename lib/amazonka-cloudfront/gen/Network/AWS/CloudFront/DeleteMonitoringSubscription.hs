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
-- Module      : Network.AWS.CloudFront.DeleteMonitoringSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables additional CloudWatch metrics for the specified CloudFront distribution.
module Network.AWS.CloudFront.DeleteMonitoringSubscription
  ( -- * Creating a Request
    deleteMonitoringSubscription,
    DeleteMonitoringSubscription,

    -- * Request Lenses
    dmsDistributionId,

    -- * Destructuring the Response
    deleteMonitoringSubscriptionResponse,
    DeleteMonitoringSubscriptionResponse,

    -- * Response Lenses
    dmsrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteMonitoringSubscription' smart constructor.
newtype DeleteMonitoringSubscription = DeleteMonitoringSubscription'
  { _dmsDistributionId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMonitoringSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmsDistributionId' - The ID of the distribution that you are disabling metrics for.
deleteMonitoringSubscription ::
  -- | 'dmsDistributionId'
  Text ->
  DeleteMonitoringSubscription
deleteMonitoringSubscription pDistributionId_ =
  DeleteMonitoringSubscription'
    { _dmsDistributionId =
        pDistributionId_
    }

-- | The ID of the distribution that you are disabling metrics for.
dmsDistributionId :: Lens' DeleteMonitoringSubscription Text
dmsDistributionId = lens _dmsDistributionId (\s a -> s {_dmsDistributionId = a})

instance AWSRequest DeleteMonitoringSubscription where
  type
    Rs DeleteMonitoringSubscription =
      DeleteMonitoringSubscriptionResponse
  request = delete cloudFront
  response =
    receiveEmpty
      ( \s h x ->
          DeleteMonitoringSubscriptionResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteMonitoringSubscription

instance NFData DeleteMonitoringSubscription

instance ToHeaders DeleteMonitoringSubscription where
  toHeaders = const mempty

instance ToPath DeleteMonitoringSubscription where
  toPath DeleteMonitoringSubscription' {..} =
    mconcat
      [ "/2020-05-31/distributions/",
        toBS _dmsDistributionId,
        "/monitoring-subscription"
      ]

instance ToQuery DeleteMonitoringSubscription where
  toQuery = const mempty

-- | /See:/ 'deleteMonitoringSubscriptionResponse' smart constructor.
newtype DeleteMonitoringSubscriptionResponse = DeleteMonitoringSubscriptionResponse'
  { _dmsrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMonitoringSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmsrsResponseStatus' - -- | The response status code.
deleteMonitoringSubscriptionResponse ::
  -- | 'dmsrsResponseStatus'
  Int ->
  DeleteMonitoringSubscriptionResponse
deleteMonitoringSubscriptionResponse pResponseStatus_ =
  DeleteMonitoringSubscriptionResponse'
    { _dmsrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dmsrsResponseStatus :: Lens' DeleteMonitoringSubscriptionResponse Int
dmsrsResponseStatus = lens _dmsrsResponseStatus (\s a -> s {_dmsrsResponseStatus = a})

instance NFData DeleteMonitoringSubscriptionResponse
