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
-- Module      : Network.AWS.CloudWatchLogs.Types.LogGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.LogGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a log group.
--
-- /See:/ 'newLogGroup' smart constructor.
data LogGroup = LogGroup'
  { retentionInDays :: Core.Maybe Core.Int,
    -- | The creation time of the log group, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the log group.
    arn :: Core.Maybe Core.Text,
    -- | The number of bytes stored.
    storedBytes :: Core.Maybe Core.Natural,
    -- | The number of metric filters.
    metricFilterCount :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
    -- data.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The name of the log group.
    logGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LogGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionInDays', 'logGroup_retentionInDays' - Undocumented member.
--
-- 'creationTime', 'logGroup_creationTime' - The creation time of the log group, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'arn', 'logGroup_arn' - The Amazon Resource Name (ARN) of the log group.
--
-- 'storedBytes', 'logGroup_storedBytes' - The number of bytes stored.
--
-- 'metricFilterCount', 'logGroup_metricFilterCount' - The number of metric filters.
--
-- 'kmsKeyId', 'logGroup_kmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data.
--
-- 'logGroupName', 'logGroup_logGroupName' - The name of the log group.
newLogGroup ::
  LogGroup
newLogGroup =
  LogGroup'
    { retentionInDays = Core.Nothing,
      creationTime = Core.Nothing,
      arn = Core.Nothing,
      storedBytes = Core.Nothing,
      metricFilterCount = Core.Nothing,
      kmsKeyId = Core.Nothing,
      logGroupName = Core.Nothing
    }

-- | Undocumented member.
logGroup_retentionInDays :: Lens.Lens' LogGroup (Core.Maybe Core.Int)
logGroup_retentionInDays = Lens.lens (\LogGroup' {retentionInDays} -> retentionInDays) (\s@LogGroup' {} a -> s {retentionInDays = a} :: LogGroup)

-- | The creation time of the log group, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
logGroup_creationTime :: Lens.Lens' LogGroup (Core.Maybe Core.Natural)
logGroup_creationTime = Lens.lens (\LogGroup' {creationTime} -> creationTime) (\s@LogGroup' {} a -> s {creationTime = a} :: LogGroup)

-- | The Amazon Resource Name (ARN) of the log group.
logGroup_arn :: Lens.Lens' LogGroup (Core.Maybe Core.Text)
logGroup_arn = Lens.lens (\LogGroup' {arn} -> arn) (\s@LogGroup' {} a -> s {arn = a} :: LogGroup)

-- | The number of bytes stored.
logGroup_storedBytes :: Lens.Lens' LogGroup (Core.Maybe Core.Natural)
logGroup_storedBytes = Lens.lens (\LogGroup' {storedBytes} -> storedBytes) (\s@LogGroup' {} a -> s {storedBytes = a} :: LogGroup)

-- | The number of metric filters.
logGroup_metricFilterCount :: Lens.Lens' LogGroup (Core.Maybe Core.Int)
logGroup_metricFilterCount = Lens.lens (\LogGroup' {metricFilterCount} -> metricFilterCount) (\s@LogGroup' {} a -> s {metricFilterCount = a} :: LogGroup)

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log
-- data.
logGroup_kmsKeyId :: Lens.Lens' LogGroup (Core.Maybe Core.Text)
logGroup_kmsKeyId = Lens.lens (\LogGroup' {kmsKeyId} -> kmsKeyId) (\s@LogGroup' {} a -> s {kmsKeyId = a} :: LogGroup)

-- | The name of the log group.
logGroup_logGroupName :: Lens.Lens' LogGroup (Core.Maybe Core.Text)
logGroup_logGroupName = Lens.lens (\LogGroup' {logGroupName} -> logGroupName) (\s@LogGroup' {} a -> s {logGroupName = a} :: LogGroup)

instance Core.FromJSON LogGroup where
  parseJSON =
    Core.withObject
      "LogGroup"
      ( \x ->
          LogGroup'
            Core.<$> (x Core..:? "retentionInDays")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "storedBytes")
            Core.<*> (x Core..:? "metricFilterCount")
            Core.<*> (x Core..:? "kmsKeyId")
            Core.<*> (x Core..:? "logGroupName")
      )

instance Core.Hashable LogGroup

instance Core.NFData LogGroup
