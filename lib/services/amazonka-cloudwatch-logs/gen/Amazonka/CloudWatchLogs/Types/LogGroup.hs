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
-- Module      : Amazonka.CloudWatchLogs.Types.LogGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.LogGroup where

import Amazonka.CloudWatchLogs.Types.DataProtectionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a log group.
--
-- /See:/ 'newLogGroup' smart constructor.
data LogGroup = LogGroup'
  { -- | The Amazon Resource Name (ARN) of the log group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The creation time of the log group, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Prelude.Maybe Prelude.Natural,
    -- | Displays whether this log group has a protection policy, or whether it
    -- had one in the past. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutDataProtectionPolicy.html PutDataProtectionPolicy>.
    dataProtectionStatus :: Prelude.Maybe DataProtectionStatus,
    -- | The Amazon Resource Name (ARN) of the KMS key to use when encrypting log
    -- data.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The number of metric filters.
    metricFilterCount :: Prelude.Maybe Prelude.Int,
    retentionInDays :: Prelude.Maybe Prelude.Int,
    -- | The number of bytes stored.
    storedBytes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'logGroup_arn' - The Amazon Resource Name (ARN) of the log group.
--
-- 'creationTime', 'logGroup_creationTime' - The creation time of the log group, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'dataProtectionStatus', 'logGroup_dataProtectionStatus' - Displays whether this log group has a protection policy, or whether it
-- had one in the past. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutDataProtectionPolicy.html PutDataProtectionPolicy>.
--
-- 'kmsKeyId', 'logGroup_kmsKeyId' - The Amazon Resource Name (ARN) of the KMS key to use when encrypting log
-- data.
--
-- 'logGroupName', 'logGroup_logGroupName' - The name of the log group.
--
-- 'metricFilterCount', 'logGroup_metricFilterCount' - The number of metric filters.
--
-- 'retentionInDays', 'logGroup_retentionInDays' - Undocumented member.
--
-- 'storedBytes', 'logGroup_storedBytes' - The number of bytes stored.
newLogGroup ::
  LogGroup
newLogGroup =
  LogGroup'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      dataProtectionStatus = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      metricFilterCount = Prelude.Nothing,
      retentionInDays = Prelude.Nothing,
      storedBytes = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the log group.
logGroup_arn :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Text)
logGroup_arn = Lens.lens (\LogGroup' {arn} -> arn) (\s@LogGroup' {} a -> s {arn = a} :: LogGroup)

-- | The creation time of the log group, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
logGroup_creationTime :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Natural)
logGroup_creationTime = Lens.lens (\LogGroup' {creationTime} -> creationTime) (\s@LogGroup' {} a -> s {creationTime = a} :: LogGroup)

-- | Displays whether this log group has a protection policy, or whether it
-- had one in the past. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutDataProtectionPolicy.html PutDataProtectionPolicy>.
logGroup_dataProtectionStatus :: Lens.Lens' LogGroup (Prelude.Maybe DataProtectionStatus)
logGroup_dataProtectionStatus = Lens.lens (\LogGroup' {dataProtectionStatus} -> dataProtectionStatus) (\s@LogGroup' {} a -> s {dataProtectionStatus = a} :: LogGroup)

-- | The Amazon Resource Name (ARN) of the KMS key to use when encrypting log
-- data.
logGroup_kmsKeyId :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Text)
logGroup_kmsKeyId = Lens.lens (\LogGroup' {kmsKeyId} -> kmsKeyId) (\s@LogGroup' {} a -> s {kmsKeyId = a} :: LogGroup)

-- | The name of the log group.
logGroup_logGroupName :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Text)
logGroup_logGroupName = Lens.lens (\LogGroup' {logGroupName} -> logGroupName) (\s@LogGroup' {} a -> s {logGroupName = a} :: LogGroup)

-- | The number of metric filters.
logGroup_metricFilterCount :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Int)
logGroup_metricFilterCount = Lens.lens (\LogGroup' {metricFilterCount} -> metricFilterCount) (\s@LogGroup' {} a -> s {metricFilterCount = a} :: LogGroup)

-- | Undocumented member.
logGroup_retentionInDays :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Int)
logGroup_retentionInDays = Lens.lens (\LogGroup' {retentionInDays} -> retentionInDays) (\s@LogGroup' {} a -> s {retentionInDays = a} :: LogGroup)

-- | The number of bytes stored.
logGroup_storedBytes :: Lens.Lens' LogGroup (Prelude.Maybe Prelude.Natural)
logGroup_storedBytes = Lens.lens (\LogGroup' {storedBytes} -> storedBytes) (\s@LogGroup' {} a -> s {storedBytes = a} :: LogGroup)

instance Data.FromJSON LogGroup where
  parseJSON =
    Data.withObject
      "LogGroup"
      ( \x ->
          LogGroup'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "dataProtectionStatus")
            Prelude.<*> (x Data..:? "kmsKeyId")
            Prelude.<*> (x Data..:? "logGroupName")
            Prelude.<*> (x Data..:? "metricFilterCount")
            Prelude.<*> (x Data..:? "retentionInDays")
            Prelude.<*> (x Data..:? "storedBytes")
      )

instance Prelude.Hashable LogGroup where
  hashWithSalt _salt LogGroup' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` dataProtectionStatus
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` metricFilterCount
      `Prelude.hashWithSalt` retentionInDays
      `Prelude.hashWithSalt` storedBytes

instance Prelude.NFData LogGroup where
  rnf LogGroup' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf dataProtectionStatus
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf metricFilterCount
      `Prelude.seq` Prelude.rnf retentionInDays
      `Prelude.seq` Prelude.rnf storedBytes
