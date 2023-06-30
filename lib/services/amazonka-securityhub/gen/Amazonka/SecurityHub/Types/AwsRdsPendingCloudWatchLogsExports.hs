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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsPendingCloudWatchLogsExports
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsPendingCloudWatchLogsExports where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies the log types to enable and disable.
--
-- /See:/ 'newAwsRdsPendingCloudWatchLogsExports' smart constructor.
data AwsRdsPendingCloudWatchLogsExports = AwsRdsPendingCloudWatchLogsExports'
  { -- | A list of log types that are being disabled.
    logTypesToDisable :: Prelude.Maybe [Prelude.Text],
    -- | A list of log types that are being enabled.
    logTypesToEnable :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsPendingCloudWatchLogsExports' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logTypesToDisable', 'awsRdsPendingCloudWatchLogsExports_logTypesToDisable' - A list of log types that are being disabled.
--
-- 'logTypesToEnable', 'awsRdsPendingCloudWatchLogsExports_logTypesToEnable' - A list of log types that are being enabled.
newAwsRdsPendingCloudWatchLogsExports ::
  AwsRdsPendingCloudWatchLogsExports
newAwsRdsPendingCloudWatchLogsExports =
  AwsRdsPendingCloudWatchLogsExports'
    { logTypesToDisable =
        Prelude.Nothing,
      logTypesToEnable = Prelude.Nothing
    }

-- | A list of log types that are being disabled.
awsRdsPendingCloudWatchLogsExports_logTypesToDisable :: Lens.Lens' AwsRdsPendingCloudWatchLogsExports (Prelude.Maybe [Prelude.Text])
awsRdsPendingCloudWatchLogsExports_logTypesToDisable = Lens.lens (\AwsRdsPendingCloudWatchLogsExports' {logTypesToDisable} -> logTypesToDisable) (\s@AwsRdsPendingCloudWatchLogsExports' {} a -> s {logTypesToDisable = a} :: AwsRdsPendingCloudWatchLogsExports) Prelude.. Lens.mapping Lens.coerced

-- | A list of log types that are being enabled.
awsRdsPendingCloudWatchLogsExports_logTypesToEnable :: Lens.Lens' AwsRdsPendingCloudWatchLogsExports (Prelude.Maybe [Prelude.Text])
awsRdsPendingCloudWatchLogsExports_logTypesToEnable = Lens.lens (\AwsRdsPendingCloudWatchLogsExports' {logTypesToEnable} -> logTypesToEnable) (\s@AwsRdsPendingCloudWatchLogsExports' {} a -> s {logTypesToEnable = a} :: AwsRdsPendingCloudWatchLogsExports) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    AwsRdsPendingCloudWatchLogsExports
  where
  parseJSON =
    Data.withObject
      "AwsRdsPendingCloudWatchLogsExports"
      ( \x ->
          AwsRdsPendingCloudWatchLogsExports'
            Prelude.<$> ( x
                            Data..:? "LogTypesToDisable"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "LogTypesToEnable"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    AwsRdsPendingCloudWatchLogsExports
  where
  hashWithSalt
    _salt
    AwsRdsPendingCloudWatchLogsExports' {..} =
      _salt
        `Prelude.hashWithSalt` logTypesToDisable
        `Prelude.hashWithSalt` logTypesToEnable

instance
  Prelude.NFData
    AwsRdsPendingCloudWatchLogsExports
  where
  rnf AwsRdsPendingCloudWatchLogsExports' {..} =
    Prelude.rnf logTypesToDisable
      `Prelude.seq` Prelude.rnf logTypesToEnable

instance
  Data.ToJSON
    AwsRdsPendingCloudWatchLogsExports
  where
  toJSON AwsRdsPendingCloudWatchLogsExports' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LogTypesToDisable" Data..=)
              Prelude.<$> logTypesToDisable,
            ("LogTypesToEnable" Data..=)
              Prelude.<$> logTypesToEnable
          ]
      )
