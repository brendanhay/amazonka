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
-- Module      : Amazonka.SecurityLake.Types.AwsLogSourceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.AwsLogSourceConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.AwsLogSourceName

-- | The Security Lake logs source configuration file describes the
-- information needed to generate Security Lake logs.
--
-- /See:/ 'newAwsLogSourceConfiguration' smart constructor.
data AwsLogSourceConfiguration = AwsLogSourceConfiguration'
  { -- | Specify the Amazon Web Services account information where you want to
    -- enable Security Lake.
    accounts :: Prelude.Maybe [Prelude.Text],
    -- | The version for a Amazon Web Services source. This must be a Regionally
    -- unique value.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | Specify the Regions where you want to enable Security Lake.
    regions :: [Prelude.Text],
    -- | The name for a Amazon Web Services source. This must be a Regionally
    -- unique value.
    sourceName :: AwsLogSourceName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLogSourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accounts', 'awsLogSourceConfiguration_accounts' - Specify the Amazon Web Services account information where you want to
-- enable Security Lake.
--
-- 'sourceVersion', 'awsLogSourceConfiguration_sourceVersion' - The version for a Amazon Web Services source. This must be a Regionally
-- unique value.
--
-- 'regions', 'awsLogSourceConfiguration_regions' - Specify the Regions where you want to enable Security Lake.
--
-- 'sourceName', 'awsLogSourceConfiguration_sourceName' - The name for a Amazon Web Services source. This must be a Regionally
-- unique value.
newAwsLogSourceConfiguration ::
  -- | 'sourceName'
  AwsLogSourceName ->
  AwsLogSourceConfiguration
newAwsLogSourceConfiguration pSourceName_ =
  AwsLogSourceConfiguration'
    { accounts =
        Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      regions = Prelude.mempty,
      sourceName = pSourceName_
    }

-- | Specify the Amazon Web Services account information where you want to
-- enable Security Lake.
awsLogSourceConfiguration_accounts :: Lens.Lens' AwsLogSourceConfiguration (Prelude.Maybe [Prelude.Text])
awsLogSourceConfiguration_accounts = Lens.lens (\AwsLogSourceConfiguration' {accounts} -> accounts) (\s@AwsLogSourceConfiguration' {} a -> s {accounts = a} :: AwsLogSourceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The version for a Amazon Web Services source. This must be a Regionally
-- unique value.
awsLogSourceConfiguration_sourceVersion :: Lens.Lens' AwsLogSourceConfiguration (Prelude.Maybe Prelude.Text)
awsLogSourceConfiguration_sourceVersion = Lens.lens (\AwsLogSourceConfiguration' {sourceVersion} -> sourceVersion) (\s@AwsLogSourceConfiguration' {} a -> s {sourceVersion = a} :: AwsLogSourceConfiguration)

-- | Specify the Regions where you want to enable Security Lake.
awsLogSourceConfiguration_regions :: Lens.Lens' AwsLogSourceConfiguration [Prelude.Text]
awsLogSourceConfiguration_regions = Lens.lens (\AwsLogSourceConfiguration' {regions} -> regions) (\s@AwsLogSourceConfiguration' {} a -> s {regions = a} :: AwsLogSourceConfiguration) Prelude.. Lens.coerced

-- | The name for a Amazon Web Services source. This must be a Regionally
-- unique value.
awsLogSourceConfiguration_sourceName :: Lens.Lens' AwsLogSourceConfiguration AwsLogSourceName
awsLogSourceConfiguration_sourceName = Lens.lens (\AwsLogSourceConfiguration' {sourceName} -> sourceName) (\s@AwsLogSourceConfiguration' {} a -> s {sourceName = a} :: AwsLogSourceConfiguration)

instance Prelude.Hashable AwsLogSourceConfiguration where
  hashWithSalt _salt AwsLogSourceConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` accounts
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` sourceName

instance Prelude.NFData AwsLogSourceConfiguration where
  rnf AwsLogSourceConfiguration' {..} =
    Prelude.rnf accounts
      `Prelude.seq` Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf sourceName

instance Data.ToJSON AwsLogSourceConfiguration where
  toJSON AwsLogSourceConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accounts" Data..=) Prelude.<$> accounts,
            ("sourceVersion" Data..=) Prelude.<$> sourceVersion,
            Prelude.Just ("regions" Data..= regions),
            Prelude.Just ("sourceName" Data..= sourceName)
          ]
      )
