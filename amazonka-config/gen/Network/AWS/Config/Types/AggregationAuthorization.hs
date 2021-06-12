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
-- Module      : Network.AWS.Config.Types.AggregationAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregationAuthorization where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object that represents the authorizations granted to aggregator
-- accounts and regions.
--
-- /See:/ 'newAggregationAuthorization' smart constructor.
data AggregationAuthorization = AggregationAuthorization'
  { -- | The time stamp when the aggregation authorization was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The 12-digit account ID of the account authorized to aggregate data.
    authorizedAccountId :: Core.Maybe Core.Text,
    -- | The region authorized to collect aggregated data.
    authorizedAwsRegion :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the aggregation object.
    aggregationAuthorizationArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AggregationAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'aggregationAuthorization_creationTime' - The time stamp when the aggregation authorization was created.
--
-- 'authorizedAccountId', 'aggregationAuthorization_authorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
--
-- 'authorizedAwsRegion', 'aggregationAuthorization_authorizedAwsRegion' - The region authorized to collect aggregated data.
--
-- 'aggregationAuthorizationArn', 'aggregationAuthorization_aggregationAuthorizationArn' - The Amazon Resource Name (ARN) of the aggregation object.
newAggregationAuthorization ::
  AggregationAuthorization
newAggregationAuthorization =
  AggregationAuthorization'
    { creationTime =
        Core.Nothing,
      authorizedAccountId = Core.Nothing,
      authorizedAwsRegion = Core.Nothing,
      aggregationAuthorizationArn = Core.Nothing
    }

-- | The time stamp when the aggregation authorization was created.
aggregationAuthorization_creationTime :: Lens.Lens' AggregationAuthorization (Core.Maybe Core.UTCTime)
aggregationAuthorization_creationTime = Lens.lens (\AggregationAuthorization' {creationTime} -> creationTime) (\s@AggregationAuthorization' {} a -> s {creationTime = a} :: AggregationAuthorization) Core.. Lens.mapping Core._Time

-- | The 12-digit account ID of the account authorized to aggregate data.
aggregationAuthorization_authorizedAccountId :: Lens.Lens' AggregationAuthorization (Core.Maybe Core.Text)
aggregationAuthorization_authorizedAccountId = Lens.lens (\AggregationAuthorization' {authorizedAccountId} -> authorizedAccountId) (\s@AggregationAuthorization' {} a -> s {authorizedAccountId = a} :: AggregationAuthorization)

-- | The region authorized to collect aggregated data.
aggregationAuthorization_authorizedAwsRegion :: Lens.Lens' AggregationAuthorization (Core.Maybe Core.Text)
aggregationAuthorization_authorizedAwsRegion = Lens.lens (\AggregationAuthorization' {authorizedAwsRegion} -> authorizedAwsRegion) (\s@AggregationAuthorization' {} a -> s {authorizedAwsRegion = a} :: AggregationAuthorization)

-- | The Amazon Resource Name (ARN) of the aggregation object.
aggregationAuthorization_aggregationAuthorizationArn :: Lens.Lens' AggregationAuthorization (Core.Maybe Core.Text)
aggregationAuthorization_aggregationAuthorizationArn = Lens.lens (\AggregationAuthorization' {aggregationAuthorizationArn} -> aggregationAuthorizationArn) (\s@AggregationAuthorization' {} a -> s {aggregationAuthorizationArn = a} :: AggregationAuthorization)

instance Core.FromJSON AggregationAuthorization where
  parseJSON =
    Core.withObject
      "AggregationAuthorization"
      ( \x ->
          AggregationAuthorization'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "AuthorizedAccountId")
            Core.<*> (x Core..:? "AuthorizedAwsRegion")
            Core.<*> (x Core..:? "AggregationAuthorizationArn")
      )

instance Core.Hashable AggregationAuthorization

instance Core.NFData AggregationAuthorization
