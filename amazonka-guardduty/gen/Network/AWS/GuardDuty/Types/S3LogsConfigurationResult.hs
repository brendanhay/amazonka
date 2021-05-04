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
-- Module      : Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.S3LogsConfigurationResult where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes whether S3 data event logs will be enabled as a data source.
--
-- /See:/ 'newS3LogsConfigurationResult' smart constructor.
data S3LogsConfigurationResult = S3LogsConfigurationResult'
  { -- | A value that describes whether S3 data event logs are automatically
    -- enabled for new members of the organization.
    status :: DataSourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON S3LogsConfigurationResult where
  parseJSON =
    Prelude.withObject
      "S3LogsConfigurationResult"
      ( \x ->
          S3LogsConfigurationResult'
            Prelude.<$> (x Prelude..: "status")
      )

instance Prelude.Hashable S3LogsConfigurationResult

instance Prelude.NFData S3LogsConfigurationResult
