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
-- Module      : Network.AWS.CostExplorer.CreateAnomalySubscription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a subscription to a cost anomaly detection monitor. You can use
-- each subscription to define subscribers with email or SNS notifications.
-- Email subscribers can set a dollar threshold and a time frequency for
-- receiving notifications.
module Network.AWS.CostExplorer.CreateAnomalySubscription
  ( -- * Creating a Request
    CreateAnomalySubscription (..),
    newCreateAnomalySubscription,

    -- * Request Lenses
    createAnomalySubscription_anomalySubscription,

    -- * Destructuring the Response
    CreateAnomalySubscriptionResponse (..),
    newCreateAnomalySubscriptionResponse,

    -- * Response Lenses
    createAnomalySubscriptionResponse_httpStatus,
    createAnomalySubscriptionResponse_subscriptionArn,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAnomalySubscription' smart constructor.
data CreateAnomalySubscription = CreateAnomalySubscription'
  { -- | The cost anomaly subscription object that you want to create.
    anomalySubscription :: AnomalySubscription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAnomalySubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalySubscription', 'createAnomalySubscription_anomalySubscription' - The cost anomaly subscription object that you want to create.
newCreateAnomalySubscription ::
  -- | 'anomalySubscription'
  AnomalySubscription ->
  CreateAnomalySubscription
newCreateAnomalySubscription pAnomalySubscription_ =
  CreateAnomalySubscription'
    { anomalySubscription =
        pAnomalySubscription_
    }

-- | The cost anomaly subscription object that you want to create.
createAnomalySubscription_anomalySubscription :: Lens.Lens' CreateAnomalySubscription AnomalySubscription
createAnomalySubscription_anomalySubscription = Lens.lens (\CreateAnomalySubscription' {anomalySubscription} -> anomalySubscription) (\s@CreateAnomalySubscription' {} a -> s {anomalySubscription = a} :: CreateAnomalySubscription)

instance Prelude.AWSRequest CreateAnomalySubscription where
  type
    Rs CreateAnomalySubscription =
      CreateAnomalySubscriptionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAnomalySubscriptionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "SubscriptionArn")
      )

instance Prelude.Hashable CreateAnomalySubscription

instance Prelude.NFData CreateAnomalySubscription

instance Prelude.ToHeaders CreateAnomalySubscription where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSInsightsIndexService.CreateAnomalySubscription" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateAnomalySubscription where
  toJSON CreateAnomalySubscription' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "AnomalySubscription"
                  Prelude..= anomalySubscription
              )
          ]
      )

instance Prelude.ToPath CreateAnomalySubscription where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateAnomalySubscription where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAnomalySubscriptionResponse' smart constructor.
data CreateAnomalySubscriptionResponse = CreateAnomalySubscriptionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of your newly created cost anomaly subscription.
    subscriptionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAnomalySubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAnomalySubscriptionResponse_httpStatus' - The response's http status code.
--
-- 'subscriptionArn', 'createAnomalySubscriptionResponse_subscriptionArn' - The unique identifier of your newly created cost anomaly subscription.
newCreateAnomalySubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'subscriptionArn'
  Prelude.Text ->
  CreateAnomalySubscriptionResponse
newCreateAnomalySubscriptionResponse
  pHttpStatus_
  pSubscriptionArn_ =
    CreateAnomalySubscriptionResponse'
      { httpStatus =
          pHttpStatus_,
        subscriptionArn = pSubscriptionArn_
      }

-- | The response's http status code.
createAnomalySubscriptionResponse_httpStatus :: Lens.Lens' CreateAnomalySubscriptionResponse Prelude.Int
createAnomalySubscriptionResponse_httpStatus = Lens.lens (\CreateAnomalySubscriptionResponse' {httpStatus} -> httpStatus) (\s@CreateAnomalySubscriptionResponse' {} a -> s {httpStatus = a} :: CreateAnomalySubscriptionResponse)

-- | The unique identifier of your newly created cost anomaly subscription.
createAnomalySubscriptionResponse_subscriptionArn :: Lens.Lens' CreateAnomalySubscriptionResponse Prelude.Text
createAnomalySubscriptionResponse_subscriptionArn = Lens.lens (\CreateAnomalySubscriptionResponse' {subscriptionArn} -> subscriptionArn) (\s@CreateAnomalySubscriptionResponse' {} a -> s {subscriptionArn = a} :: CreateAnomalySubscriptionResponse)

instance
  Prelude.NFData
    CreateAnomalySubscriptionResponse
