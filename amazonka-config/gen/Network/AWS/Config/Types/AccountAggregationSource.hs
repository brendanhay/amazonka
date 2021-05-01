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
-- Module      : Network.AWS.Config.Types.AccountAggregationSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AccountAggregationSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A collection of accounts and regions.
--
-- /See:/ 'newAccountAggregationSource' smart constructor.
data AccountAggregationSource = AccountAggregationSource'
  { -- | If true, aggregate existing AWS Config regions and future regions.
    allAwsRegions :: Prelude.Maybe Prelude.Bool,
    -- | The source regions being aggregated.
    awsRegions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The 12-digit account ID of the account being aggregated.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccountAggregationSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allAwsRegions', 'accountAggregationSource_allAwsRegions' - If true, aggregate existing AWS Config regions and future regions.
--
-- 'awsRegions', 'accountAggregationSource_awsRegions' - The source regions being aggregated.
--
-- 'accountIds', 'accountAggregationSource_accountIds' - The 12-digit account ID of the account being aggregated.
newAccountAggregationSource ::
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  AccountAggregationSource
newAccountAggregationSource pAccountIds_ =
  AccountAggregationSource'
    { allAwsRegions =
        Prelude.Nothing,
      awsRegions = Prelude.Nothing,
      accountIds = Prelude._Coerce Lens.# pAccountIds_
    }

-- | If true, aggregate existing AWS Config regions and future regions.
accountAggregationSource_allAwsRegions :: Lens.Lens' AccountAggregationSource (Prelude.Maybe Prelude.Bool)
accountAggregationSource_allAwsRegions = Lens.lens (\AccountAggregationSource' {allAwsRegions} -> allAwsRegions) (\s@AccountAggregationSource' {} a -> s {allAwsRegions = a} :: AccountAggregationSource)

-- | The source regions being aggregated.
accountAggregationSource_awsRegions :: Lens.Lens' AccountAggregationSource (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
accountAggregationSource_awsRegions = Lens.lens (\AccountAggregationSource' {awsRegions} -> awsRegions) (\s@AccountAggregationSource' {} a -> s {awsRegions = a} :: AccountAggregationSource) Prelude.. Lens.mapping Prelude._Coerce

-- | The 12-digit account ID of the account being aggregated.
accountAggregationSource_accountIds :: Lens.Lens' AccountAggregationSource (Prelude.NonEmpty Prelude.Text)
accountAggregationSource_accountIds = Lens.lens (\AccountAggregationSource' {accountIds} -> accountIds) (\s@AccountAggregationSource' {} a -> s {accountIds = a} :: AccountAggregationSource) Prelude.. Prelude._Coerce

instance Prelude.FromJSON AccountAggregationSource where
  parseJSON =
    Prelude.withObject
      "AccountAggregationSource"
      ( \x ->
          AccountAggregationSource'
            Prelude.<$> (x Prelude..:? "AllAwsRegions")
            Prelude.<*> (x Prelude..:? "AwsRegions")
            Prelude.<*> (x Prelude..: "AccountIds")
      )

instance Prelude.Hashable AccountAggregationSource

instance Prelude.NFData AccountAggregationSource

instance Prelude.ToJSON AccountAggregationSource where
  toJSON AccountAggregationSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AllAwsRegions" Prelude..=)
              Prelude.<$> allAwsRegions,
            ("AwsRegions" Prelude..=) Prelude.<$> awsRegions,
            Prelude.Just ("AccountIds" Prelude..= accountIds)
          ]
      )
