{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateAnomalySubscription' smart constructor.
data UpdateAnomalySubscription = UpdateAnomalySubscription'
  { -- | The update to the threshold value for receiving notifications.
    threshold :: Prelude.Maybe Prelude.Double,
    -- | The subscription\'s new name.
    subscriptionName :: Prelude.Maybe Prelude.Text,
    -- | The update to the subscriber list.
    subscribers :: Prelude.Maybe [Subscriber],
    -- | The update to the frequency value at which subscribers will receive
    -- notifications.
    frequency :: Prelude.Maybe AnomalySubscriptionFrequency,
    -- | A list of cost anomaly monitor ARNs.
    monitorArnList :: Prelude.Maybe [Prelude.Text],
    -- | A cost anomaly subscription Amazon Resource Name (ARN).
    subscriptionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateAnomalySubscription
newUpdateAnomalySubscription pSubscriptionArn_ =
  UpdateAnomalySubscription'
    { threshold =
        Prelude.Nothing,
      subscriptionName = Prelude.Nothing,
      subscribers = Prelude.Nothing,
      frequency = Prelude.Nothing,
      monitorArnList = Prelude.Nothing,
      subscriptionArn = pSubscriptionArn_
    }

-- | The update to the threshold value for receiving notifications.
updateAnomalySubscription_threshold :: Lens.Lens' UpdateAnomalySubscription (Prelude.Maybe Prelude.Double)
updateAnomalySubscription_threshold = Lens.lens (\UpdateAnomalySubscription' {threshold} -> threshold) (\s@UpdateAnomalySubscription' {} a -> s {threshold = a} :: UpdateAnomalySubscription)

-- | The subscription\'s new name.
updateAnomalySubscription_subscriptionName :: Lens.Lens' UpdateAnomalySubscription (Prelude.Maybe Prelude.Text)
updateAnomalySubscription_subscriptionName = Lens.lens (\UpdateAnomalySubscription' {subscriptionName} -> subscriptionName) (\s@UpdateAnomalySubscription' {} a -> s {subscriptionName = a} :: UpdateAnomalySubscription)

-- | The update to the subscriber list.
updateAnomalySubscription_subscribers :: Lens.Lens' UpdateAnomalySubscription (Prelude.Maybe [Subscriber])
updateAnomalySubscription_subscribers = Lens.lens (\UpdateAnomalySubscription' {subscribers} -> subscribers) (\s@UpdateAnomalySubscription' {} a -> s {subscribers = a} :: UpdateAnomalySubscription) Prelude.. Lens.mapping Prelude._Coerce

-- | The update to the frequency value at which subscribers will receive
-- notifications.
updateAnomalySubscription_frequency :: Lens.Lens' UpdateAnomalySubscription (Prelude.Maybe AnomalySubscriptionFrequency)
updateAnomalySubscription_frequency = Lens.lens (\UpdateAnomalySubscription' {frequency} -> frequency) (\s@UpdateAnomalySubscription' {} a -> s {frequency = a} :: UpdateAnomalySubscription)

-- | A list of cost anomaly monitor ARNs.
updateAnomalySubscription_monitorArnList :: Lens.Lens' UpdateAnomalySubscription (Prelude.Maybe [Prelude.Text])
updateAnomalySubscription_monitorArnList = Lens.lens (\UpdateAnomalySubscription' {monitorArnList} -> monitorArnList) (\s@UpdateAnomalySubscription' {} a -> s {monitorArnList = a} :: UpdateAnomalySubscription) Prelude.. Lens.mapping Prelude._Coerce

-- | A cost anomaly subscription Amazon Resource Name (ARN).
updateAnomalySubscription_subscriptionArn :: Lens.Lens' UpdateAnomalySubscription Prelude.Text
updateAnomalySubscription_subscriptionArn = Lens.lens (\UpdateAnomalySubscription' {subscriptionArn} -> subscriptionArn) (\s@UpdateAnomalySubscription' {} a -> s {subscriptionArn = a} :: UpdateAnomalySubscription)

instance Prelude.AWSRequest UpdateAnomalySubscription where
  type
    Rs UpdateAnomalySubscription =
      UpdateAnomalySubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAnomalySubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "SubscriptionArn")
      )

instance Prelude.Hashable UpdateAnomalySubscription

instance Prelude.NFData UpdateAnomalySubscription

instance Prelude.ToHeaders UpdateAnomalySubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSInsightsIndexService.UpdateAnomalySubscription" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateAnomalySubscription where
  toJSON UpdateAnomalySubscription' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Threshold" Prelude..=) Prelude.<$> threshold,
            ("SubscriptionName" Prelude..=)
              Prelude.<$> subscriptionName,
            ("Subscribers" Prelude..=) Prelude.<$> subscribers,
            ("Frequency" Prelude..=) Prelude.<$> frequency,
            ("MonitorArnList" Prelude..=)
              Prelude.<$> monitorArnList,
            Prelude.Just
              ("SubscriptionArn" Prelude..= subscriptionArn)
          ]
      )

instance Prelude.ToPath UpdateAnomalySubscription where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateAnomalySubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAnomalySubscriptionResponse' smart constructor.
data UpdateAnomalySubscriptionResponse = UpdateAnomalySubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A cost anomaly subscription ARN.
    subscriptionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'subscriptionArn'
  Prelude.Text ->
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
updateAnomalySubscriptionResponse_httpStatus :: Lens.Lens' UpdateAnomalySubscriptionResponse Prelude.Int
updateAnomalySubscriptionResponse_httpStatus = Lens.lens (\UpdateAnomalySubscriptionResponse' {httpStatus} -> httpStatus) (\s@UpdateAnomalySubscriptionResponse' {} a -> s {httpStatus = a} :: UpdateAnomalySubscriptionResponse)

-- | A cost anomaly subscription ARN.
updateAnomalySubscriptionResponse_subscriptionArn :: Lens.Lens' UpdateAnomalySubscriptionResponse Prelude.Text
updateAnomalySubscriptionResponse_subscriptionArn = Lens.lens (\UpdateAnomalySubscriptionResponse' {subscriptionArn} -> subscriptionArn) (\s@UpdateAnomalySubscriptionResponse' {} a -> s {subscriptionArn = a} :: UpdateAnomalySubscriptionResponse)

instance
  Prelude.NFData
    UpdateAnomalySubscriptionResponse
