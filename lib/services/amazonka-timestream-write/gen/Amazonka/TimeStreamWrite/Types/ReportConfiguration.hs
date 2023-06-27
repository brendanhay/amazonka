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
-- Module      : Amazonka.TimeStreamWrite.Types.ReportConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.ReportConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.ReportS3Configuration

-- | Report configuration for a batch load task. This contains details about
-- where error reports are stored.
--
-- /See:/ 'newReportConfiguration' smart constructor.
data ReportConfiguration = ReportConfiguration'
  { -- | Configuration of an S3 location to write error reports and events for a
    -- batch load.
    reportS3Configuration :: Prelude.Maybe ReportS3Configuration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportS3Configuration', 'reportConfiguration_reportS3Configuration' - Configuration of an S3 location to write error reports and events for a
-- batch load.
newReportConfiguration ::
  ReportConfiguration
newReportConfiguration =
  ReportConfiguration'
    { reportS3Configuration =
        Prelude.Nothing
    }

-- | Configuration of an S3 location to write error reports and events for a
-- batch load.
reportConfiguration_reportS3Configuration :: Lens.Lens' ReportConfiguration (Prelude.Maybe ReportS3Configuration)
reportConfiguration_reportS3Configuration = Lens.lens (\ReportConfiguration' {reportS3Configuration} -> reportS3Configuration) (\s@ReportConfiguration' {} a -> s {reportS3Configuration = a} :: ReportConfiguration)

instance Data.FromJSON ReportConfiguration where
  parseJSON =
    Data.withObject
      "ReportConfiguration"
      ( \x ->
          ReportConfiguration'
            Prelude.<$> (x Data..:? "ReportS3Configuration")
      )

instance Prelude.Hashable ReportConfiguration where
  hashWithSalt _salt ReportConfiguration' {..} =
    _salt `Prelude.hashWithSalt` reportS3Configuration

instance Prelude.NFData ReportConfiguration where
  rnf ReportConfiguration' {..} =
    Prelude.rnf reportS3Configuration

instance Data.ToJSON ReportConfiguration where
  toJSON ReportConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ReportS3Configuration" Data..=)
              Prelude.<$> reportS3Configuration
          ]
      )
