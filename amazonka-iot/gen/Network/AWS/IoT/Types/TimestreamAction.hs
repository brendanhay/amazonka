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
-- Module      : Network.AWS.IoT.Types.TimestreamAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimestreamAction where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.TimestreamDimension
import Network.AWS.IoT.Types.TimestreamTimestamp
import qualified Network.AWS.Lens as Lens

-- | The Timestream rule action writes attributes (measures) from an MQTT
-- message into an Amazon Timestream table. For more information, see the
-- <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream>
-- topic rule action documentation.
--
-- /See:/ 'newTimestreamAction' smart constructor.
data TimestreamAction = TimestreamAction'
  { -- | Specifies an application-defined value to replace the default value
    -- assigned to the Timestream record\'s timestamp in the @time@ column.
    --
    -- You can use this property to specify the value and the precision of the
    -- Timestream record\'s timestamp. You can specify a value from the message
    -- payload or a value computed by a substitution template.
    --
    -- If omitted, the topic rule action assigns the timestamp, in
    -- milliseconds, at the time it processed the rule.
    timestamp :: Core.Maybe TimestreamTimestamp,
    -- | The ARN of the role that grants permission to write to the Amazon
    -- Timestream database table.
    roleArn :: Core.Text,
    -- | The name of an Amazon Timestream database.
    databaseName :: Core.Text,
    -- | The name of the database table into which to write the measure records.
    tableName :: Core.Text,
    -- | Metadata attributes of the time series that are written in each measure
    -- record.
    dimensions :: Core.NonEmpty TimestreamDimension
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TimestreamAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timestamp', 'timestreamAction_timestamp' - Specifies an application-defined value to replace the default value
-- assigned to the Timestream record\'s timestamp in the @time@ column.
--
-- You can use this property to specify the value and the precision of the
-- Timestream record\'s timestamp. You can specify a value from the message
-- payload or a value computed by a substitution template.
--
-- If omitted, the topic rule action assigns the timestamp, in
-- milliseconds, at the time it processed the rule.
--
-- 'roleArn', 'timestreamAction_roleArn' - The ARN of the role that grants permission to write to the Amazon
-- Timestream database table.
--
-- 'databaseName', 'timestreamAction_databaseName' - The name of an Amazon Timestream database.
--
-- 'tableName', 'timestreamAction_tableName' - The name of the database table into which to write the measure records.
--
-- 'dimensions', 'timestreamAction_dimensions' - Metadata attributes of the time series that are written in each measure
-- record.
newTimestreamAction ::
  -- | 'roleArn'
  Core.Text ->
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  -- | 'dimensions'
  Core.NonEmpty TimestreamDimension ->
  TimestreamAction
newTimestreamAction
  pRoleArn_
  pDatabaseName_
  pTableName_
  pDimensions_ =
    TimestreamAction'
      { timestamp = Core.Nothing,
        roleArn = pRoleArn_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        dimensions = Lens._Coerce Lens.# pDimensions_
      }

-- | Specifies an application-defined value to replace the default value
-- assigned to the Timestream record\'s timestamp in the @time@ column.
--
-- You can use this property to specify the value and the precision of the
-- Timestream record\'s timestamp. You can specify a value from the message
-- payload or a value computed by a substitution template.
--
-- If omitted, the topic rule action assigns the timestamp, in
-- milliseconds, at the time it processed the rule.
timestreamAction_timestamp :: Lens.Lens' TimestreamAction (Core.Maybe TimestreamTimestamp)
timestreamAction_timestamp = Lens.lens (\TimestreamAction' {timestamp} -> timestamp) (\s@TimestreamAction' {} a -> s {timestamp = a} :: TimestreamAction)

-- | The ARN of the role that grants permission to write to the Amazon
-- Timestream database table.
timestreamAction_roleArn :: Lens.Lens' TimestreamAction Core.Text
timestreamAction_roleArn = Lens.lens (\TimestreamAction' {roleArn} -> roleArn) (\s@TimestreamAction' {} a -> s {roleArn = a} :: TimestreamAction)

-- | The name of an Amazon Timestream database.
timestreamAction_databaseName :: Lens.Lens' TimestreamAction Core.Text
timestreamAction_databaseName = Lens.lens (\TimestreamAction' {databaseName} -> databaseName) (\s@TimestreamAction' {} a -> s {databaseName = a} :: TimestreamAction)

-- | The name of the database table into which to write the measure records.
timestreamAction_tableName :: Lens.Lens' TimestreamAction Core.Text
timestreamAction_tableName = Lens.lens (\TimestreamAction' {tableName} -> tableName) (\s@TimestreamAction' {} a -> s {tableName = a} :: TimestreamAction)

-- | Metadata attributes of the time series that are written in each measure
-- record.
timestreamAction_dimensions :: Lens.Lens' TimestreamAction (Core.NonEmpty TimestreamDimension)
timestreamAction_dimensions = Lens.lens (\TimestreamAction' {dimensions} -> dimensions) (\s@TimestreamAction' {} a -> s {dimensions = a} :: TimestreamAction) Core.. Lens._Coerce

instance Core.FromJSON TimestreamAction where
  parseJSON =
    Core.withObject
      "TimestreamAction"
      ( \x ->
          TimestreamAction'
            Core.<$> (x Core..:? "timestamp")
            Core.<*> (x Core..: "roleArn")
            Core.<*> (x Core..: "databaseName")
            Core.<*> (x Core..: "tableName")
            Core.<*> (x Core..: "dimensions")
      )

instance Core.Hashable TimestreamAction

instance Core.NFData TimestreamAction

instance Core.ToJSON TimestreamAction where
  toJSON TimestreamAction' {..} =
    Core.object
      ( Core.catMaybes
          [ ("timestamp" Core..=) Core.<$> timestamp,
            Core.Just ("roleArn" Core..= roleArn),
            Core.Just ("databaseName" Core..= databaseName),
            Core.Just ("tableName" Core..= tableName),
            Core.Just ("dimensions" Core..= dimensions)
          ]
      )
