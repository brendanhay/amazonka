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
-- Module      : Amazonka.EC2.Types.VerifiedAccessLogCloudWatchLogsDestinationOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessLogCloudWatchLogsDestinationOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Options for CloudWatch Logs as a logging destination.
--
-- /See:/ 'newVerifiedAccessLogCloudWatchLogsDestinationOptions' smart constructor.
data VerifiedAccessLogCloudWatchLogsDestinationOptions = VerifiedAccessLogCloudWatchLogsDestinationOptions'
  { -- | The ID of the CloudWatch Logs log group.
    logGroup :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether logging is enabled.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifiedAccessLogCloudWatchLogsDestinationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroup', 'verifiedAccessLogCloudWatchLogsDestinationOptions_logGroup' - The ID of the CloudWatch Logs log group.
--
-- 'enabled', 'verifiedAccessLogCloudWatchLogsDestinationOptions_enabled' - Indicates whether logging is enabled.
newVerifiedAccessLogCloudWatchLogsDestinationOptions ::
  -- | 'enabled'
  Prelude.Bool ->
  VerifiedAccessLogCloudWatchLogsDestinationOptions
newVerifiedAccessLogCloudWatchLogsDestinationOptions
  pEnabled_ =
    VerifiedAccessLogCloudWatchLogsDestinationOptions'
      { logGroup =
          Prelude.Nothing,
        enabled = pEnabled_
      }

-- | The ID of the CloudWatch Logs log group.
verifiedAccessLogCloudWatchLogsDestinationOptions_logGroup :: Lens.Lens' VerifiedAccessLogCloudWatchLogsDestinationOptions (Prelude.Maybe Prelude.Text)
verifiedAccessLogCloudWatchLogsDestinationOptions_logGroup = Lens.lens (\VerifiedAccessLogCloudWatchLogsDestinationOptions' {logGroup} -> logGroup) (\s@VerifiedAccessLogCloudWatchLogsDestinationOptions' {} a -> s {logGroup = a} :: VerifiedAccessLogCloudWatchLogsDestinationOptions)

-- | Indicates whether logging is enabled.
verifiedAccessLogCloudWatchLogsDestinationOptions_enabled :: Lens.Lens' VerifiedAccessLogCloudWatchLogsDestinationOptions Prelude.Bool
verifiedAccessLogCloudWatchLogsDestinationOptions_enabled = Lens.lens (\VerifiedAccessLogCloudWatchLogsDestinationOptions' {enabled} -> enabled) (\s@VerifiedAccessLogCloudWatchLogsDestinationOptions' {} a -> s {enabled = a} :: VerifiedAccessLogCloudWatchLogsDestinationOptions)

instance
  Prelude.Hashable
    VerifiedAccessLogCloudWatchLogsDestinationOptions
  where
  hashWithSalt
    _salt
    VerifiedAccessLogCloudWatchLogsDestinationOptions' {..} =
      _salt
        `Prelude.hashWithSalt` logGroup
        `Prelude.hashWithSalt` enabled

instance
  Prelude.NFData
    VerifiedAccessLogCloudWatchLogsDestinationOptions
  where
  rnf
    VerifiedAccessLogCloudWatchLogsDestinationOptions' {..} =
      Prelude.rnf logGroup
        `Prelude.seq` Prelude.rnf enabled

instance
  Data.ToQuery
    VerifiedAccessLogCloudWatchLogsDestinationOptions
  where
  toQuery
    VerifiedAccessLogCloudWatchLogsDestinationOptions' {..} =
      Prelude.mconcat
        [ "LogGroup" Data.=: logGroup,
          "Enabled" Data.=: enabled
        ]
