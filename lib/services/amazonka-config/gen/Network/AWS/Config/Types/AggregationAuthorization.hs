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
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents the authorizations granted to aggregator
-- accounts and regions.
--
-- /See:/ 'newAggregationAuthorization' smart constructor.
data AggregationAuthorization = AggregationAuthorization'
  { -- | The time stamp when the aggregation authorization was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The region authorized to collect aggregated data.
    authorizedAwsRegion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the aggregation object.
    aggregationAuthorizationArn :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account ID of the account authorized to aggregate data.
    authorizedAccountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'authorizedAwsRegion', 'aggregationAuthorization_authorizedAwsRegion' - The region authorized to collect aggregated data.
--
-- 'aggregationAuthorizationArn', 'aggregationAuthorization_aggregationAuthorizationArn' - The Amazon Resource Name (ARN) of the aggregation object.
--
-- 'authorizedAccountId', 'aggregationAuthorization_authorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
newAggregationAuthorization ::
  AggregationAuthorization
newAggregationAuthorization =
  AggregationAuthorization'
    { creationTime =
        Prelude.Nothing,
      authorizedAwsRegion = Prelude.Nothing,
      aggregationAuthorizationArn = Prelude.Nothing,
      authorizedAccountId = Prelude.Nothing
    }

-- | The time stamp when the aggregation authorization was created.
aggregationAuthorization_creationTime :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.UTCTime)
aggregationAuthorization_creationTime = Lens.lens (\AggregationAuthorization' {creationTime} -> creationTime) (\s@AggregationAuthorization' {} a -> s {creationTime = a} :: AggregationAuthorization) Prelude.. Lens.mapping Core._Time

-- | The region authorized to collect aggregated data.
aggregationAuthorization_authorizedAwsRegion :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.Text)
aggregationAuthorization_authorizedAwsRegion = Lens.lens (\AggregationAuthorization' {authorizedAwsRegion} -> authorizedAwsRegion) (\s@AggregationAuthorization' {} a -> s {authorizedAwsRegion = a} :: AggregationAuthorization)

-- | The Amazon Resource Name (ARN) of the aggregation object.
aggregationAuthorization_aggregationAuthorizationArn :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.Text)
aggregationAuthorization_aggregationAuthorizationArn = Lens.lens (\AggregationAuthorization' {aggregationAuthorizationArn} -> aggregationAuthorizationArn) (\s@AggregationAuthorization' {} a -> s {aggregationAuthorizationArn = a} :: AggregationAuthorization)

-- | The 12-digit account ID of the account authorized to aggregate data.
aggregationAuthorization_authorizedAccountId :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.Text)
aggregationAuthorization_authorizedAccountId = Lens.lens (\AggregationAuthorization' {authorizedAccountId} -> authorizedAccountId) (\s@AggregationAuthorization' {} a -> s {authorizedAccountId = a} :: AggregationAuthorization)

instance Core.FromJSON AggregationAuthorization where
  parseJSON =
    Core.withObject
      "AggregationAuthorization"
      ( \x ->
          AggregationAuthorization'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "AuthorizedAwsRegion")
            Prelude.<*> (x Core..:? "AggregationAuthorizationArn")
            Prelude.<*> (x Core..:? "AuthorizedAccountId")
      )

instance Prelude.Hashable AggregationAuthorization

instance Prelude.NFData AggregationAuthorization
