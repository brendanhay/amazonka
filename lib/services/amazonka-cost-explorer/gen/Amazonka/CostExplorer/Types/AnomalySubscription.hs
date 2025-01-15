{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CostExplorer.Types.AnomalySubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.AnomalySubscription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.AnomalySubscriptionFrequency
import Amazonka.CostExplorer.Types.Expression
import Amazonka.CostExplorer.Types.Subscriber
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The association between a monitor, threshold, and list of subscribers
-- used to deliver notifications about anomalies detected by a monitor that
-- exceeds a threshold. The content consists of the detailed metadata and
-- the current status of the @AnomalySubscription@ object.
--
-- /See:/ 'newAnomalySubscription' smart constructor.
data AnomalySubscription = AnomalySubscription'
  { -- | Your unique account identifier.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The @AnomalySubscription@ Amazon Resource Name (ARN).
    subscriptionArn :: Prelude.Maybe Prelude.Text,
    -- | (deprecated)
    --
    -- The dollar value that triggers a notification if the threshold is
    -- exceeded.
    --
    -- This field has been deprecated. To specify a threshold, use
    -- ThresholdExpression. Continued use of Threshold will be treated as
    -- shorthand syntax for a ThresholdExpression.
    --
    -- One of Threshold or ThresholdExpression is required for this resource.
    threshold :: Prelude.Maybe Prelude.Double,
    -- | An
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
    -- object used to specify the anomalies that you want to generate alerts
    -- for. This supports dimensions and nested expressions. The supported
    -- dimensions are @ANOMALY_TOTAL_IMPACT_ABSOLUTE@ and
    -- @ANOMALY_TOTAL_IMPACT_PERCENTAGE@. The supported nested expression types
    -- are @AND@ and @OR@. The match option @GREATER_THAN_OR_EQUAL@ is
    -- required. Values must be numbers between 0 and 10,000,000,000.
    --
    -- One of Threshold or ThresholdExpression is required for this resource.
    --
    -- The following are examples of valid ThresholdExpressions:
    --
    -- -   Absolute threshold:
    --     @{ \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_ABSOLUTE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }@
    --
    -- -   Percentage threshold:
    --     @{ \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_PERCENTAGE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }@
    --
    -- -   @AND@ two thresholds together:
    --     @{ \"And\": [ { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_ABSOLUTE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }, { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_PERCENTAGE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } } ] }@
    --
    -- -   @OR@ two thresholds together:
    --     @{ \"Or\": [ { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_ABSOLUTE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }, { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_PERCENTAGE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } } ] }@
    thresholdExpression :: Prelude.Maybe Expression,
    -- | A list of cost anomaly monitors.
    monitorArnList :: [Prelude.Text],
    -- | A list of subscribers to notify.
    subscribers :: [Subscriber],
    -- | The frequency that anomaly reports are sent over email.
    frequency :: AnomalySubscriptionFrequency,
    -- | The name for the subscription.
    subscriptionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalySubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'anomalySubscription_accountId' - Your unique account identifier.
--
-- 'subscriptionArn', 'anomalySubscription_subscriptionArn' - The @AnomalySubscription@ Amazon Resource Name (ARN).
--
-- 'threshold', 'anomalySubscription_threshold' - (deprecated)
--
-- The dollar value that triggers a notification if the threshold is
-- exceeded.
--
-- This field has been deprecated. To specify a threshold, use
-- ThresholdExpression. Continued use of Threshold will be treated as
-- shorthand syntax for a ThresholdExpression.
--
-- One of Threshold or ThresholdExpression is required for this resource.
--
-- 'thresholdExpression', 'anomalySubscription_thresholdExpression' - An
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object used to specify the anomalies that you want to generate alerts
-- for. This supports dimensions and nested expressions. The supported
-- dimensions are @ANOMALY_TOTAL_IMPACT_ABSOLUTE@ and
-- @ANOMALY_TOTAL_IMPACT_PERCENTAGE@. The supported nested expression types
-- are @AND@ and @OR@. The match option @GREATER_THAN_OR_EQUAL@ is
-- required. Values must be numbers between 0 and 10,000,000,000.
--
-- One of Threshold or ThresholdExpression is required for this resource.
--
-- The following are examples of valid ThresholdExpressions:
--
-- -   Absolute threshold:
--     @{ \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_ABSOLUTE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }@
--
-- -   Percentage threshold:
--     @{ \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_PERCENTAGE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }@
--
-- -   @AND@ two thresholds together:
--     @{ \"And\": [ { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_ABSOLUTE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }, { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_PERCENTAGE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } } ] }@
--
-- -   @OR@ two thresholds together:
--     @{ \"Or\": [ { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_ABSOLUTE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }, { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_PERCENTAGE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } } ] }@
--
-- 'monitorArnList', 'anomalySubscription_monitorArnList' - A list of cost anomaly monitors.
--
-- 'subscribers', 'anomalySubscription_subscribers' - A list of subscribers to notify.
--
-- 'frequency', 'anomalySubscription_frequency' - The frequency that anomaly reports are sent over email.
--
-- 'subscriptionName', 'anomalySubscription_subscriptionName' - The name for the subscription.
newAnomalySubscription ::
  -- | 'frequency'
  AnomalySubscriptionFrequency ->
  -- | 'subscriptionName'
  Prelude.Text ->
  AnomalySubscription
newAnomalySubscription pFrequency_ pSubscriptionName_ =
  AnomalySubscription'
    { accountId = Prelude.Nothing,
      subscriptionArn = Prelude.Nothing,
      threshold = Prelude.Nothing,
      thresholdExpression = Prelude.Nothing,
      monitorArnList = Prelude.mempty,
      subscribers = Prelude.mempty,
      frequency = pFrequency_,
      subscriptionName = pSubscriptionName_
    }

-- | Your unique account identifier.
anomalySubscription_accountId :: Lens.Lens' AnomalySubscription (Prelude.Maybe Prelude.Text)
anomalySubscription_accountId = Lens.lens (\AnomalySubscription' {accountId} -> accountId) (\s@AnomalySubscription' {} a -> s {accountId = a} :: AnomalySubscription)

-- | The @AnomalySubscription@ Amazon Resource Name (ARN).
anomalySubscription_subscriptionArn :: Lens.Lens' AnomalySubscription (Prelude.Maybe Prelude.Text)
anomalySubscription_subscriptionArn = Lens.lens (\AnomalySubscription' {subscriptionArn} -> subscriptionArn) (\s@AnomalySubscription' {} a -> s {subscriptionArn = a} :: AnomalySubscription)

-- | (deprecated)
--
-- The dollar value that triggers a notification if the threshold is
-- exceeded.
--
-- This field has been deprecated. To specify a threshold, use
-- ThresholdExpression. Continued use of Threshold will be treated as
-- shorthand syntax for a ThresholdExpression.
--
-- One of Threshold or ThresholdExpression is required for this resource.
anomalySubscription_threshold :: Lens.Lens' AnomalySubscription (Prelude.Maybe Prelude.Double)
anomalySubscription_threshold = Lens.lens (\AnomalySubscription' {threshold} -> threshold) (\s@AnomalySubscription' {} a -> s {threshold = a} :: AnomalySubscription)

-- | An
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression>
-- object used to specify the anomalies that you want to generate alerts
-- for. This supports dimensions and nested expressions. The supported
-- dimensions are @ANOMALY_TOTAL_IMPACT_ABSOLUTE@ and
-- @ANOMALY_TOTAL_IMPACT_PERCENTAGE@. The supported nested expression types
-- are @AND@ and @OR@. The match option @GREATER_THAN_OR_EQUAL@ is
-- required. Values must be numbers between 0 and 10,000,000,000.
--
-- One of Threshold or ThresholdExpression is required for this resource.
--
-- The following are examples of valid ThresholdExpressions:
--
-- -   Absolute threshold:
--     @{ \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_ABSOLUTE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }@
--
-- -   Percentage threshold:
--     @{ \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_PERCENTAGE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }@
--
-- -   @AND@ two thresholds together:
--     @{ \"And\": [ { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_ABSOLUTE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }, { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_PERCENTAGE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } } ] }@
--
-- -   @OR@ two thresholds together:
--     @{ \"Or\": [ { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_ABSOLUTE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } }, { \"Dimensions\": { \"Key\": \"ANOMALY_TOTAL_IMPACT_PERCENTAGE\", \"MatchOptions\": [ \"GREATER_THAN_OR_EQUAL\" ], \"Values\": [ \"100\" ] } } ] }@
anomalySubscription_thresholdExpression :: Lens.Lens' AnomalySubscription (Prelude.Maybe Expression)
anomalySubscription_thresholdExpression = Lens.lens (\AnomalySubscription' {thresholdExpression} -> thresholdExpression) (\s@AnomalySubscription' {} a -> s {thresholdExpression = a} :: AnomalySubscription)

-- | A list of cost anomaly monitors.
anomalySubscription_monitorArnList :: Lens.Lens' AnomalySubscription [Prelude.Text]
anomalySubscription_monitorArnList = Lens.lens (\AnomalySubscription' {monitorArnList} -> monitorArnList) (\s@AnomalySubscription' {} a -> s {monitorArnList = a} :: AnomalySubscription) Prelude.. Lens.coerced

-- | A list of subscribers to notify.
anomalySubscription_subscribers :: Lens.Lens' AnomalySubscription [Subscriber]
anomalySubscription_subscribers = Lens.lens (\AnomalySubscription' {subscribers} -> subscribers) (\s@AnomalySubscription' {} a -> s {subscribers = a} :: AnomalySubscription) Prelude.. Lens.coerced

-- | The frequency that anomaly reports are sent over email.
anomalySubscription_frequency :: Lens.Lens' AnomalySubscription AnomalySubscriptionFrequency
anomalySubscription_frequency = Lens.lens (\AnomalySubscription' {frequency} -> frequency) (\s@AnomalySubscription' {} a -> s {frequency = a} :: AnomalySubscription)

-- | The name for the subscription.
anomalySubscription_subscriptionName :: Lens.Lens' AnomalySubscription Prelude.Text
anomalySubscription_subscriptionName = Lens.lens (\AnomalySubscription' {subscriptionName} -> subscriptionName) (\s@AnomalySubscription' {} a -> s {subscriptionName = a} :: AnomalySubscription)

instance Data.FromJSON AnomalySubscription where
  parseJSON =
    Data.withObject
      "AnomalySubscription"
      ( \x ->
          AnomalySubscription'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "SubscriptionArn")
            Prelude.<*> (x Data..:? "Threshold")
            Prelude.<*> (x Data..:? "ThresholdExpression")
            Prelude.<*> (x Data..:? "MonitorArnList" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Subscribers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Frequency")
            Prelude.<*> (x Data..: "SubscriptionName")
      )

instance Prelude.Hashable AnomalySubscription where
  hashWithSalt _salt AnomalySubscription' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` subscriptionArn
      `Prelude.hashWithSalt` threshold
      `Prelude.hashWithSalt` thresholdExpression
      `Prelude.hashWithSalt` monitorArnList
      `Prelude.hashWithSalt` subscribers
      `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` subscriptionName

instance Prelude.NFData AnomalySubscription where
  rnf AnomalySubscription' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf subscriptionArn `Prelude.seq`
        Prelude.rnf threshold `Prelude.seq`
          Prelude.rnf thresholdExpression `Prelude.seq`
            Prelude.rnf monitorArnList `Prelude.seq`
              Prelude.rnf subscribers `Prelude.seq`
                Prelude.rnf frequency `Prelude.seq`
                  Prelude.rnf subscriptionName

instance Data.ToJSON AnomalySubscription where
  toJSON AnomalySubscription' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountId" Data..=) Prelude.<$> accountId,
            ("SubscriptionArn" Data..=)
              Prelude.<$> subscriptionArn,
            ("Threshold" Data..=) Prelude.<$> threshold,
            ("ThresholdExpression" Data..=)
              Prelude.<$> thresholdExpression,
            Prelude.Just
              ("MonitorArnList" Data..= monitorArnList),
            Prelude.Just ("Subscribers" Data..= subscribers),
            Prelude.Just ("Frequency" Data..= frequency),
            Prelude.Just
              ("SubscriptionName" Data..= subscriptionName)
          ]
      )
