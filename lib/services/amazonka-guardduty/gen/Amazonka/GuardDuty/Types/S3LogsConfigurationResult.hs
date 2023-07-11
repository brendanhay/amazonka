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
-- Module      : Amazonka.GuardDuty.Types.S3LogsConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.S3LogsConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DataSourceStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes whether S3 data event logs will be enabled as a data source.
--
-- /See:/ 'newS3LogsConfigurationResult' smart constructor.
data S3LogsConfigurationResult = S3LogsConfigurationResult'
  { -- | A value that describes whether S3 data event logs are automatically
    -- enabled for new members of the organization.
    status :: DataSourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3LogsConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 's3LogsConfigurationResult_status' - A value that describes whether S3 data event logs are automatically
-- enabled for new members of the organization.
newS3LogsConfigurationResult ::
  -- | 'status'
  DataSourceStatus ->
  S3LogsConfigurationResult
newS3LogsConfigurationResult pStatus_ =
  S3LogsConfigurationResult' {status = pStatus_}

-- | A value that describes whether S3 data event logs are automatically
-- enabled for new members of the organization.
s3LogsConfigurationResult_status :: Lens.Lens' S3LogsConfigurationResult DataSourceStatus
s3LogsConfigurationResult_status = Lens.lens (\S3LogsConfigurationResult' {status} -> status) (\s@S3LogsConfigurationResult' {} a -> s {status = a} :: S3LogsConfigurationResult)

instance Data.FromJSON S3LogsConfigurationResult where
  parseJSON =
    Data.withObject
      "S3LogsConfigurationResult"
      ( \x ->
          S3LogsConfigurationResult'
            Prelude.<$> (x Data..: "status")
      )

instance Prelude.Hashable S3LogsConfigurationResult where
  hashWithSalt _salt S3LogsConfigurationResult' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData S3LogsConfigurationResult where
  rnf S3LogsConfigurationResult' {..} =
    Prelude.rnf status
