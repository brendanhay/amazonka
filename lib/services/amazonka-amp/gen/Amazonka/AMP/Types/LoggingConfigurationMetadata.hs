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
-- Module      : Amazonka.AMP.Types.LoggingConfigurationMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.LoggingConfigurationMetadata where

import Amazonka.AMP.Types.LoggingConfigurationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a logging configuration metadata.
--
-- /See:/ 'newLoggingConfigurationMetadata' smart constructor.
data LoggingConfigurationMetadata = LoggingConfigurationMetadata'
  { -- | The time when the logging configuration was created.
    createdAt :: Data.POSIX,
    -- | The ARN of the CW log group to which the vended log data will be
    -- published.
    logGroupArn :: Prelude.Text,
    -- | The time when the logging configuration was modified.
    modifiedAt :: Data.POSIX,
    -- | The status of the logging configuration.
    status :: LoggingConfigurationStatus,
    -- | The workspace where the logging configuration exists.
    workspace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingConfigurationMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'loggingConfigurationMetadata_createdAt' - The time when the logging configuration was created.
--
-- 'logGroupArn', 'loggingConfigurationMetadata_logGroupArn' - The ARN of the CW log group to which the vended log data will be
-- published.
--
-- 'modifiedAt', 'loggingConfigurationMetadata_modifiedAt' - The time when the logging configuration was modified.
--
-- 'status', 'loggingConfigurationMetadata_status' - The status of the logging configuration.
--
-- 'workspace', 'loggingConfigurationMetadata_workspace' - The workspace where the logging configuration exists.
newLoggingConfigurationMetadata ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'logGroupArn'
  Prelude.Text ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  -- | 'status'
  LoggingConfigurationStatus ->
  -- | 'workspace'
  Prelude.Text ->
  LoggingConfigurationMetadata
newLoggingConfigurationMetadata
  pCreatedAt_
  pLogGroupArn_
  pModifiedAt_
  pStatus_
  pWorkspace_ =
    LoggingConfigurationMetadata'
      { createdAt =
          Data._Time Lens.# pCreatedAt_,
        logGroupArn = pLogGroupArn_,
        modifiedAt = Data._Time Lens.# pModifiedAt_,
        status = pStatus_,
        workspace = pWorkspace_
      }

-- | The time when the logging configuration was created.
loggingConfigurationMetadata_createdAt :: Lens.Lens' LoggingConfigurationMetadata Prelude.UTCTime
loggingConfigurationMetadata_createdAt = Lens.lens (\LoggingConfigurationMetadata' {createdAt} -> createdAt) (\s@LoggingConfigurationMetadata' {} a -> s {createdAt = a} :: LoggingConfigurationMetadata) Prelude.. Data._Time

-- | The ARN of the CW log group to which the vended log data will be
-- published.
loggingConfigurationMetadata_logGroupArn :: Lens.Lens' LoggingConfigurationMetadata Prelude.Text
loggingConfigurationMetadata_logGroupArn = Lens.lens (\LoggingConfigurationMetadata' {logGroupArn} -> logGroupArn) (\s@LoggingConfigurationMetadata' {} a -> s {logGroupArn = a} :: LoggingConfigurationMetadata)

-- | The time when the logging configuration was modified.
loggingConfigurationMetadata_modifiedAt :: Lens.Lens' LoggingConfigurationMetadata Prelude.UTCTime
loggingConfigurationMetadata_modifiedAt = Lens.lens (\LoggingConfigurationMetadata' {modifiedAt} -> modifiedAt) (\s@LoggingConfigurationMetadata' {} a -> s {modifiedAt = a} :: LoggingConfigurationMetadata) Prelude.. Data._Time

-- | The status of the logging configuration.
loggingConfigurationMetadata_status :: Lens.Lens' LoggingConfigurationMetadata LoggingConfigurationStatus
loggingConfigurationMetadata_status = Lens.lens (\LoggingConfigurationMetadata' {status} -> status) (\s@LoggingConfigurationMetadata' {} a -> s {status = a} :: LoggingConfigurationMetadata)

-- | The workspace where the logging configuration exists.
loggingConfigurationMetadata_workspace :: Lens.Lens' LoggingConfigurationMetadata Prelude.Text
loggingConfigurationMetadata_workspace = Lens.lens (\LoggingConfigurationMetadata' {workspace} -> workspace) (\s@LoggingConfigurationMetadata' {} a -> s {workspace = a} :: LoggingConfigurationMetadata)

instance Data.FromJSON LoggingConfigurationMetadata where
  parseJSON =
    Data.withObject
      "LoggingConfigurationMetadata"
      ( \x ->
          LoggingConfigurationMetadata'
            Prelude.<$> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "logGroupArn")
            Prelude.<*> (x Data..: "modifiedAt")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "workspace")
      )

instance
  Prelude.Hashable
    LoggingConfigurationMetadata
  where
  hashWithSalt _salt LoggingConfigurationMetadata' {..} =
    _salt `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` logGroupArn
      `Prelude.hashWithSalt` modifiedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` workspace

instance Prelude.NFData LoggingConfigurationMetadata where
  rnf LoggingConfigurationMetadata' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf logGroupArn
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf workspace
