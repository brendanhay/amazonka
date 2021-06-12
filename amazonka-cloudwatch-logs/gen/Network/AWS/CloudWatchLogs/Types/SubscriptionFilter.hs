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
-- Module      : Network.AWS.CloudWatchLogs.Types.SubscriptionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.SubscriptionFilter where

import Network.AWS.CloudWatchLogs.Types.Distribution
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a subscription filter.
--
-- /See:/ 'newSubscriptionFilter' smart constructor.
data SubscriptionFilter = SubscriptionFilter'
  { -- | The name of the subscription filter.
    filterName :: Core.Maybe Core.Text,
    -- | The creation time of the subscription filter, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Core.Maybe Core.Text,
    roleArn :: Core.Maybe Core.Text,
    filterPattern :: Core.Maybe Core.Text,
    distribution :: Core.Maybe Distribution,
    -- | The name of the log group.
    logGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubscriptionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterName', 'subscriptionFilter_filterName' - The name of the subscription filter.
--
-- 'creationTime', 'subscriptionFilter_creationTime' - The creation time of the subscription filter, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'destinationArn', 'subscriptionFilter_destinationArn' - The Amazon Resource Name (ARN) of the destination.
--
-- 'roleArn', 'subscriptionFilter_roleArn' -
--
-- 'filterPattern', 'subscriptionFilter_filterPattern' - Undocumented member.
--
-- 'distribution', 'subscriptionFilter_distribution' - Undocumented member.
--
-- 'logGroupName', 'subscriptionFilter_logGroupName' - The name of the log group.
newSubscriptionFilter ::
  SubscriptionFilter
newSubscriptionFilter =
  SubscriptionFilter'
    { filterName = Core.Nothing,
      creationTime = Core.Nothing,
      destinationArn = Core.Nothing,
      roleArn = Core.Nothing,
      filterPattern = Core.Nothing,
      distribution = Core.Nothing,
      logGroupName = Core.Nothing
    }

-- | The name of the subscription filter.
subscriptionFilter_filterName :: Lens.Lens' SubscriptionFilter (Core.Maybe Core.Text)
subscriptionFilter_filterName = Lens.lens (\SubscriptionFilter' {filterName} -> filterName) (\s@SubscriptionFilter' {} a -> s {filterName = a} :: SubscriptionFilter)

-- | The creation time of the subscription filter, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
subscriptionFilter_creationTime :: Lens.Lens' SubscriptionFilter (Core.Maybe Core.Natural)
subscriptionFilter_creationTime = Lens.lens (\SubscriptionFilter' {creationTime} -> creationTime) (\s@SubscriptionFilter' {} a -> s {creationTime = a} :: SubscriptionFilter)

-- | The Amazon Resource Name (ARN) of the destination.
subscriptionFilter_destinationArn :: Lens.Lens' SubscriptionFilter (Core.Maybe Core.Text)
subscriptionFilter_destinationArn = Lens.lens (\SubscriptionFilter' {destinationArn} -> destinationArn) (\s@SubscriptionFilter' {} a -> s {destinationArn = a} :: SubscriptionFilter)

-- |
subscriptionFilter_roleArn :: Lens.Lens' SubscriptionFilter (Core.Maybe Core.Text)
subscriptionFilter_roleArn = Lens.lens (\SubscriptionFilter' {roleArn} -> roleArn) (\s@SubscriptionFilter' {} a -> s {roleArn = a} :: SubscriptionFilter)

-- | Undocumented member.
subscriptionFilter_filterPattern :: Lens.Lens' SubscriptionFilter (Core.Maybe Core.Text)
subscriptionFilter_filterPattern = Lens.lens (\SubscriptionFilter' {filterPattern} -> filterPattern) (\s@SubscriptionFilter' {} a -> s {filterPattern = a} :: SubscriptionFilter)

-- | Undocumented member.
subscriptionFilter_distribution :: Lens.Lens' SubscriptionFilter (Core.Maybe Distribution)
subscriptionFilter_distribution = Lens.lens (\SubscriptionFilter' {distribution} -> distribution) (\s@SubscriptionFilter' {} a -> s {distribution = a} :: SubscriptionFilter)

-- | The name of the log group.
subscriptionFilter_logGroupName :: Lens.Lens' SubscriptionFilter (Core.Maybe Core.Text)
subscriptionFilter_logGroupName = Lens.lens (\SubscriptionFilter' {logGroupName} -> logGroupName) (\s@SubscriptionFilter' {} a -> s {logGroupName = a} :: SubscriptionFilter)

instance Core.FromJSON SubscriptionFilter where
  parseJSON =
    Core.withObject
      "SubscriptionFilter"
      ( \x ->
          SubscriptionFilter'
            Core.<$> (x Core..:? "filterName")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "destinationArn")
            Core.<*> (x Core..:? "roleArn")
            Core.<*> (x Core..:? "filterPattern")
            Core.<*> (x Core..:? "distribution")
            Core.<*> (x Core..:? "logGroupName")
      )

instance Core.Hashable SubscriptionFilter

instance Core.NFData SubscriptionFilter
