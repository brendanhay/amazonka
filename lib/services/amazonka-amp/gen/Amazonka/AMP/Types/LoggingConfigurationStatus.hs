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
-- Module      : Amazonka.AMP.Types.LoggingConfigurationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.LoggingConfigurationStatus where

import Amazonka.AMP.Types.LoggingConfigurationStatusCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the status of a logging configuration.
--
-- /See:/ 'newLoggingConfigurationStatus' smart constructor.
data LoggingConfigurationStatus = LoggingConfigurationStatus'
  { -- | The reason for failure if any.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | Status code of the logging configuration.
    statusCode :: LoggingConfigurationStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingConfigurationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusReason', 'loggingConfigurationStatus_statusReason' - The reason for failure if any.
--
-- 'statusCode', 'loggingConfigurationStatus_statusCode' - Status code of the logging configuration.
newLoggingConfigurationStatus ::
  -- | 'statusCode'
  LoggingConfigurationStatusCode ->
  LoggingConfigurationStatus
newLoggingConfigurationStatus pStatusCode_ =
  LoggingConfigurationStatus'
    { statusReason =
        Prelude.Nothing,
      statusCode = pStatusCode_
    }

-- | The reason for failure if any.
loggingConfigurationStatus_statusReason :: Lens.Lens' LoggingConfigurationStatus (Prelude.Maybe Prelude.Text)
loggingConfigurationStatus_statusReason = Lens.lens (\LoggingConfigurationStatus' {statusReason} -> statusReason) (\s@LoggingConfigurationStatus' {} a -> s {statusReason = a} :: LoggingConfigurationStatus)

-- | Status code of the logging configuration.
loggingConfigurationStatus_statusCode :: Lens.Lens' LoggingConfigurationStatus LoggingConfigurationStatusCode
loggingConfigurationStatus_statusCode = Lens.lens (\LoggingConfigurationStatus' {statusCode} -> statusCode) (\s@LoggingConfigurationStatus' {} a -> s {statusCode = a} :: LoggingConfigurationStatus)

instance Core.FromJSON LoggingConfigurationStatus where
  parseJSON =
    Core.withObject
      "LoggingConfigurationStatus"
      ( \x ->
          LoggingConfigurationStatus'
            Prelude.<$> (x Core..:? "statusReason")
            Prelude.<*> (x Core..: "statusCode")
      )

instance Prelude.Hashable LoggingConfigurationStatus where
  hashWithSalt _salt LoggingConfigurationStatus' {..} =
    _salt `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData LoggingConfigurationStatus where
  rnf LoggingConfigurationStatus' {..} =
    Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf statusCode
