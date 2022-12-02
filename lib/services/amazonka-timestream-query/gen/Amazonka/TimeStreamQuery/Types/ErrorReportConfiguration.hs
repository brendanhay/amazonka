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
-- Module      : Amazonka.TimeStreamQuery.Types.ErrorReportConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.ErrorReportConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.S3Configuration

-- | Configuration required for error reporting.
--
-- /See:/ 'newErrorReportConfiguration' smart constructor.
data ErrorReportConfiguration = ErrorReportConfiguration'
  { -- | The S3 configuration for the error reports.
    s3Configuration :: S3Configuration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorReportConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Configuration', 'errorReportConfiguration_s3Configuration' - The S3 configuration for the error reports.
newErrorReportConfiguration ::
  -- | 's3Configuration'
  S3Configuration ->
  ErrorReportConfiguration
newErrorReportConfiguration pS3Configuration_ =
  ErrorReportConfiguration'
    { s3Configuration =
        pS3Configuration_
    }

-- | The S3 configuration for the error reports.
errorReportConfiguration_s3Configuration :: Lens.Lens' ErrorReportConfiguration S3Configuration
errorReportConfiguration_s3Configuration = Lens.lens (\ErrorReportConfiguration' {s3Configuration} -> s3Configuration) (\s@ErrorReportConfiguration' {} a -> s {s3Configuration = a} :: ErrorReportConfiguration)

instance Data.FromJSON ErrorReportConfiguration where
  parseJSON =
    Data.withObject
      "ErrorReportConfiguration"
      ( \x ->
          ErrorReportConfiguration'
            Prelude.<$> (x Data..: "S3Configuration")
      )

instance Prelude.Hashable ErrorReportConfiguration where
  hashWithSalt _salt ErrorReportConfiguration' {..} =
    _salt `Prelude.hashWithSalt` s3Configuration

instance Prelude.NFData ErrorReportConfiguration where
  rnf ErrorReportConfiguration' {..} =
    Prelude.rnf s3Configuration

instance Data.ToJSON ErrorReportConfiguration where
  toJSON ErrorReportConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("S3Configuration" Data..= s3Configuration)
          ]
      )
