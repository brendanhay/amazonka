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
-- Module      : Amazonka.OsIs.Types.LogPublishingOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.LogPublishingOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OsIs.Types.CloudWatchLogDestination
import qualified Amazonka.Prelude as Prelude

-- | Container for the values required to configure logging for the pipeline.
-- If you don\'t specify these values, OpenSearch Ingestion will not
-- publish logs from your application to CloudWatch Logs.
--
-- /See:/ 'newLogPublishingOptions' smart constructor.
data LogPublishingOptions = LogPublishingOptions'
  { -- | The destination for OpenSearch Ingestion logs sent to Amazon CloudWatch
    -- Logs. This parameter is required if @IsLoggingEnabled@ is set to @true@.
    cloudWatchLogDestination :: Prelude.Maybe CloudWatchLogDestination,
    -- | Whether logs should be published.
    isLoggingEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogPublishingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogDestination', 'logPublishingOptions_cloudWatchLogDestination' - The destination for OpenSearch Ingestion logs sent to Amazon CloudWatch
-- Logs. This parameter is required if @IsLoggingEnabled@ is set to @true@.
--
-- 'isLoggingEnabled', 'logPublishingOptions_isLoggingEnabled' - Whether logs should be published.
newLogPublishingOptions ::
  LogPublishingOptions
newLogPublishingOptions =
  LogPublishingOptions'
    { cloudWatchLogDestination =
        Prelude.Nothing,
      isLoggingEnabled = Prelude.Nothing
    }

-- | The destination for OpenSearch Ingestion logs sent to Amazon CloudWatch
-- Logs. This parameter is required if @IsLoggingEnabled@ is set to @true@.
logPublishingOptions_cloudWatchLogDestination :: Lens.Lens' LogPublishingOptions (Prelude.Maybe CloudWatchLogDestination)
logPublishingOptions_cloudWatchLogDestination = Lens.lens (\LogPublishingOptions' {cloudWatchLogDestination} -> cloudWatchLogDestination) (\s@LogPublishingOptions' {} a -> s {cloudWatchLogDestination = a} :: LogPublishingOptions)

-- | Whether logs should be published.
logPublishingOptions_isLoggingEnabled :: Lens.Lens' LogPublishingOptions (Prelude.Maybe Prelude.Bool)
logPublishingOptions_isLoggingEnabled = Lens.lens (\LogPublishingOptions' {isLoggingEnabled} -> isLoggingEnabled) (\s@LogPublishingOptions' {} a -> s {isLoggingEnabled = a} :: LogPublishingOptions)

instance Data.FromJSON LogPublishingOptions where
  parseJSON =
    Data.withObject
      "LogPublishingOptions"
      ( \x ->
          LogPublishingOptions'
            Prelude.<$> (x Data..:? "CloudWatchLogDestination")
            Prelude.<*> (x Data..:? "IsLoggingEnabled")
      )

instance Prelude.Hashable LogPublishingOptions where
  hashWithSalt _salt LogPublishingOptions' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogDestination
      `Prelude.hashWithSalt` isLoggingEnabled

instance Prelude.NFData LogPublishingOptions where
  rnf LogPublishingOptions' {..} =
    Prelude.rnf cloudWatchLogDestination
      `Prelude.seq` Prelude.rnf isLoggingEnabled

instance Data.ToJSON LogPublishingOptions where
  toJSON LogPublishingOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogDestination" Data..=)
              Prelude.<$> cloudWatchLogDestination,
            ("IsLoggingEnabled" Data..=)
              Prelude.<$> isLoggingEnabled
          ]
      )
