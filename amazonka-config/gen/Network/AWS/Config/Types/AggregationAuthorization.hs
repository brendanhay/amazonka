{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents the authorizations granted to aggregator
-- accounts and regions.
--
-- /See:/ 'newAggregationAuthorization' smart constructor.
data AggregationAuthorization = AggregationAuthorization'
  { -- | The time stamp when the aggregation authorization was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The 12-digit account ID of the account authorized to aggregate data.
    authorizedAccountId :: Prelude.Maybe Prelude.Text,
    -- | The region authorized to collect aggregated data.
    authorizedAwsRegion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the aggregation object.
    aggregationAuthorizationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      authorizedAccountId = Prelude.Nothing,
      authorizedAwsRegion = Prelude.Nothing,
      aggregationAuthorizationArn = Prelude.Nothing
    }

-- | The time stamp when the aggregation authorization was created.
aggregationAuthorization_creationTime :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.UTCTime)
aggregationAuthorization_creationTime = Lens.lens (\AggregationAuthorization' {creationTime} -> creationTime) (\s@AggregationAuthorization' {} a -> s {creationTime = a} :: AggregationAuthorization) Prelude.. Lens.mapping Prelude._Time

-- | The 12-digit account ID of the account authorized to aggregate data.
aggregationAuthorization_authorizedAccountId :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.Text)
aggregationAuthorization_authorizedAccountId = Lens.lens (\AggregationAuthorization' {authorizedAccountId} -> authorizedAccountId) (\s@AggregationAuthorization' {} a -> s {authorizedAccountId = a} :: AggregationAuthorization)

-- | The region authorized to collect aggregated data.
aggregationAuthorization_authorizedAwsRegion :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.Text)
aggregationAuthorization_authorizedAwsRegion = Lens.lens (\AggregationAuthorization' {authorizedAwsRegion} -> authorizedAwsRegion) (\s@AggregationAuthorization' {} a -> s {authorizedAwsRegion = a} :: AggregationAuthorization)

-- | The Amazon Resource Name (ARN) of the aggregation object.
aggregationAuthorization_aggregationAuthorizationArn :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.Text)
aggregationAuthorization_aggregationAuthorizationArn = Lens.lens (\AggregationAuthorization' {aggregationAuthorizationArn} -> aggregationAuthorizationArn) (\s@AggregationAuthorization' {} a -> s {aggregationAuthorizationArn = a} :: AggregationAuthorization)

instance Prelude.FromJSON AggregationAuthorization where
  parseJSON =
    Prelude.withObject
      "AggregationAuthorization"
      ( \x ->
          AggregationAuthorization'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "AuthorizedAccountId")
            Prelude.<*> (x Prelude..:? "AuthorizedAwsRegion")
            Prelude.<*> (x Prelude..:? "AggregationAuthorizationArn")
      )

instance Prelude.Hashable AggregationAuthorization

instance Prelude.NFData AggregationAuthorization
