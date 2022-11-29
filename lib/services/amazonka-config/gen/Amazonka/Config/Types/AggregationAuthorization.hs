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
-- Module      : Amazonka.Config.Types.AggregationAuthorization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregationAuthorization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the authorizations granted to aggregator
-- accounts and regions.
--
-- /See:/ 'newAggregationAuthorization' smart constructor.
data AggregationAuthorization = AggregationAuthorization'
  { -- | The region authorized to collect aggregated data.
    authorizedAwsRegion :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account ID of the account authorized to aggregate data.
    authorizedAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the aggregation object.
    aggregationAuthorizationArn :: Prelude.Maybe Prelude.Text,
    -- | The time stamp when the aggregation authorization was created.
    creationTime :: Prelude.Maybe Core.POSIX
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
-- 'authorizedAwsRegion', 'aggregationAuthorization_authorizedAwsRegion' - The region authorized to collect aggregated data.
--
-- 'authorizedAccountId', 'aggregationAuthorization_authorizedAccountId' - The 12-digit account ID of the account authorized to aggregate data.
--
-- 'aggregationAuthorizationArn', 'aggregationAuthorization_aggregationAuthorizationArn' - The Amazon Resource Name (ARN) of the aggregation object.
--
-- 'creationTime', 'aggregationAuthorization_creationTime' - The time stamp when the aggregation authorization was created.
newAggregationAuthorization ::
  AggregationAuthorization
newAggregationAuthorization =
  AggregationAuthorization'
    { authorizedAwsRegion =
        Prelude.Nothing,
      authorizedAccountId = Prelude.Nothing,
      aggregationAuthorizationArn = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The region authorized to collect aggregated data.
aggregationAuthorization_authorizedAwsRegion :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.Text)
aggregationAuthorization_authorizedAwsRegion = Lens.lens (\AggregationAuthorization' {authorizedAwsRegion} -> authorizedAwsRegion) (\s@AggregationAuthorization' {} a -> s {authorizedAwsRegion = a} :: AggregationAuthorization)

-- | The 12-digit account ID of the account authorized to aggregate data.
aggregationAuthorization_authorizedAccountId :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.Text)
aggregationAuthorization_authorizedAccountId = Lens.lens (\AggregationAuthorization' {authorizedAccountId} -> authorizedAccountId) (\s@AggregationAuthorization' {} a -> s {authorizedAccountId = a} :: AggregationAuthorization)

-- | The Amazon Resource Name (ARN) of the aggregation object.
aggregationAuthorization_aggregationAuthorizationArn :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.Text)
aggregationAuthorization_aggregationAuthorizationArn = Lens.lens (\AggregationAuthorization' {aggregationAuthorizationArn} -> aggregationAuthorizationArn) (\s@AggregationAuthorization' {} a -> s {aggregationAuthorizationArn = a} :: AggregationAuthorization)

-- | The time stamp when the aggregation authorization was created.
aggregationAuthorization_creationTime :: Lens.Lens' AggregationAuthorization (Prelude.Maybe Prelude.UTCTime)
aggregationAuthorization_creationTime = Lens.lens (\AggregationAuthorization' {creationTime} -> creationTime) (\s@AggregationAuthorization' {} a -> s {creationTime = a} :: AggregationAuthorization) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON AggregationAuthorization where
  parseJSON =
    Core.withObject
      "AggregationAuthorization"
      ( \x ->
          AggregationAuthorization'
            Prelude.<$> (x Core..:? "AuthorizedAwsRegion")
            Prelude.<*> (x Core..:? "AuthorizedAccountId")
            Prelude.<*> (x Core..:? "AggregationAuthorizationArn")
            Prelude.<*> (x Core..:? "CreationTime")
      )

instance Prelude.Hashable AggregationAuthorization where
  hashWithSalt _salt AggregationAuthorization' {..} =
    _salt `Prelude.hashWithSalt` authorizedAwsRegion
      `Prelude.hashWithSalt` authorizedAccountId
      `Prelude.hashWithSalt` aggregationAuthorizationArn
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData AggregationAuthorization where
  rnf AggregationAuthorization' {..} =
    Prelude.rnf authorizedAwsRegion
      `Prelude.seq` Prelude.rnf authorizedAccountId
      `Prelude.seq` Prelude.rnf aggregationAuthorizationArn
      `Prelude.seq` Prelude.rnf creationTime
