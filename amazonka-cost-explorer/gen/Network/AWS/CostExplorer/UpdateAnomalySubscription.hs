{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.UpdateAnomalySubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing cost anomaly monitor subscription.
module Network.AWS.CostExplorer.UpdateAnomalySubscription
  ( -- * Creating a Request
    UpdateAnomalySubscription (..),
    newUpdateAnomalySubscription,

    -- * Request Lenses
    updateAnomalySubscription_threshold,
    updateAnomalySubscription_subscriptionName,
    updateAnomalySubscription_subscribers,
    updateAnomalySubscription_frequency,
    updateAnomalySubscription_monitorArnList,
    updateAnomalySubscription_subscriptionArn,

    -- * Destructuring the Response
    UpdateAnomalySubscriptionResponse (..),
    newUpdateAnomalySubscriptionResponse,

    -- * Response Lenses
    updateAnomalySubscriptionResponse_httpStatus,
    updateAnomalySubscriptionResponse_subscriptionArn,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAnomalySubscription' smart constructor.
data UpdateAnomalySubscription = UpdateAnomalySubscription'
  { -- | The update to the threshold value for receiving notifications.
    threshold :: Core.Maybe Core.Double,
    -- | The subscription\'s new name.
    subscriptionName :: Core.Maybe Core.Text,
    -- | The update to the subscriber list.
    subscribers :: Core.Maybe [Subscriber],
    -- | The update to the frequency value at which subscribers will receive
    -- notifications.
    frequency :: Core.Maybe AnomalySubscriptionFrequency,
    -- | A list of cost anomaly monitor ARNs.
    monitorArnList :: Core.Maybe [Core.Text],
    -- | A cost anomaly subscription Amazon Resource Name (ARN).
    subscriptionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAnomalySubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'threshold', 'updateAnomalySubscription_threshold' - The update to the threshold value for receiving notifications.
--
-- 'subscriptionName', 'updateAnomalySubscription_subscriptionName' - The subscription\'s new name.
--
-- 'subscribers', 'updateAnomalySubscription_subscribers' - The update to the subscriber list.
--
-- 'frequency', 'updateAnomalySubscription_frequency' - The update to the frequency value at which subscribers will receive
-- notifications.
--
-- 'monitorArnList', 'updateAnomalySubscription_monitorArnList' - A list of cost anomaly monitor ARNs.
--
-- 'subscriptionArn', 'updateAnomalySubscription_subscriptionArn' - A cost anomaly subscription Amazon Resource Name (ARN).
newUpdateAnomalySubscription ::
  -- | 'subscriptionArn'
  Core.Text ->
  UpdateAnomalySubscription
newUpdateAnomalySubscription pSubscriptionArn_ =
  UpdateAnomalySubscription'
    { threshold =
        Core.Nothing,
      subscriptionName = Core.Nothing,
      subscribers = Core.Nothing,
      frequency = Core.Nothing,
      monitorArnList = Core.Nothing,
      subscriptionArn = pSubscriptionArn_
    }

-- | The update to the threshold value for receiving notifications.
updateAnomalySubscription_threshold :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe Core.Double)
updateAnomalySubscription_threshold = Lens.lens (\UpdateAnomalySubscription' {threshold} -> threshold) (\s@UpdateAnomalySubscription' {} a -> s {threshold = a} :: UpdateAnomalySubscription)

-- | The subscription\'s new name.
updateAnomalySubscription_subscriptionName :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe Core.Text)
updateAnomalySubscription_subscriptionName = Lens.lens (\UpdateAnomalySubscription' {subscriptionName} -> subscriptionName) (\s@UpdateAnomalySubscription' {} a -> s {subscriptionName = a} :: UpdateAnomalySubscription)

-- | The update to the subscriber list.
updateAnomalySubscription_subscribers :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe [Subscriber])
updateAnomalySubscription_subscribers = Lens.lens (\UpdateAnomalySubscription' {subscribers} -> subscribers) (\s@UpdateAnomalySubscription' {} a -> s {subscribers = a} :: UpdateAnomalySubscription) Core.. Lens.mapping Lens._Coerce

-- | The update to the frequency value at which subscribers will receive
-- notifications.
updateAnomalySubscription_frequency :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe AnomalySubscriptionFrequency)
updateAnomalySubscription_frequency = Lens.lens (\UpdateAnomalySubscription' {frequency} -> frequency) (\s@UpdateAnomalySubscription' {} a -> s {frequency = a} :: UpdateAnomalySubscription)

-- | A list of cost anomaly monitor ARNs.
updateAnomalySubscription_monitorArnList :: Lens.Lens' UpdateAnomalySubscription (Core.Maybe [Core.Text])
updateAnomalySubscription_monitorArnList = Lens.lens (\UpdateAnomalySubscription' {monitorArnList} -> monitorArnList) (\s@UpdateAnomalySubscription' {} a -> s {monitorArnList = a} :: UpdateAnomalySubscription) Core.. Lens.mapping Lens._Coerce

-- | A cost anomaly subscription Amazon Resource Name (ARN).
updateAnomalySubscription_subscriptionArn :: Lens.Lens' UpdateAnomalySubscription Core.Text
updateAnomalySubscription_subscriptionArn = Lens.lens (\UpdateAnomalySubscription' {subscriptionArn} -> subscriptionArn) (\s@UpdateAnomalySubscription' {} a -> s {subscriptionArn = a} :: UpdateAnomalySubscription)

instance Core.AWSRequest UpdateAnomalySubscription where
  type
    AWSResponse UpdateAnomalySubscription =
      UpdateAnomalySubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAnomalySubscriptionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "SubscriptionArn")
      )

instance Core.Hashable UpdateAnomalySubscription

instance Core.NFData UpdateAnomalySubscription

instance Core.ToHeaders UpdateAnomalySubscription where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.UpdateAnomalySubscription" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateAnomalySubscription where
  toJSON UpdateAnomalySubscription' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Threshold" Core..=) Core.<$> threshold,
            ("SubscriptionName" Core..=)
              Core.<$> subscriptionName,
            ("Subscribers" Core..=) Core.<$> subscribers,
            ("Frequency" Core..=) Core.<$> frequency,
            ("MonitorArnList" Core..=) Core.<$> monitorArnList,
            Core.Just
              ("SubscriptionArn" Core..= subscriptionArn)
          ]
      )

instance Core.ToPath UpdateAnomalySubscription where
  toPath = Core.const "/"

instance Core.ToQuery UpdateAnomalySubscription where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateAnomalySubscriptionResponse' smart constructor.
data UpdateAnomalySubscriptionResponse = UpdateAnomalySubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A cost anomaly subscription ARN.
    subscriptionArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateAnomalySubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAnomalySubscriptionResponse_httpStatus' - The response's http status code.
--
-- 'subscriptionArn', 'updateAnomalySubscriptionResponse_subscriptionArn' - A cost anomaly subscription ARN.
newUpdateAnomalySubscriptionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'subscriptionArn'
  Core.Text ->
  UpdateAnomalySubscriptionResponse
newUpdateAnomalySubscriptionResponse
  pHttpStatus_
  pSubscriptionArn_ =
    UpdateAnomalySubscriptionResponse'
      { httpStatus =
          pHttpStatus_,
        subscriptionArn = pSubscriptionArn_
      }

-- | The response's http status code.
updateAnomalySubscriptionResponse_httpStatus :: Lens.Lens' UpdateAnomalySubscriptionResponse Core.Int
updateAnomalySubscriptionResponse_httpStatus = Lens.lens (\UpdateAnomalySubscriptionResponse' {httpStatus} -> httpStatus) (\s@UpdateAnomalySubscriptionResponse' {} a -> s {httpStatus = a} :: UpdateAnomalySubscriptionResponse)

-- | A cost anomaly subscription ARN.
updateAnomalySubscriptionResponse_subscriptionArn :: Lens.Lens' UpdateAnomalySubscriptionResponse Core.Text
updateAnomalySubscriptionResponse_subscriptionArn = Lens.lens (\UpdateAnomalySubscriptionResponse' {subscriptionArn} -> subscriptionArn) (\s@UpdateAnomalySubscriptionResponse' {} a -> s {subscriptionArn = a} :: UpdateAnomalySubscriptionResponse)

instance
  Core.NFData
    UpdateAnomalySubscriptionResponse
