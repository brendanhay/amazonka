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
-- Module      : Amazonka.IoT.Types.TimestreamAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TimestreamAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.TimestreamDimension
import Amazonka.IoT.Types.TimestreamTimestamp
import qualified Amazonka.Prelude as Prelude

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
    timestamp :: Prelude.Maybe TimestreamTimestamp,
    -- | The ARN of the role that grants permission to write to the Amazon
    -- Timestream database table.
    roleArn :: Prelude.Text,
    -- | The name of an Amazon Timestream database.
    databaseName :: Prelude.Text,
    -- | The name of the database table into which to write the measure records.
    tableName :: Prelude.Text,
    -- | Metadata attributes of the time series that are written in each measure
    -- record.
    dimensions :: Prelude.NonEmpty TimestreamDimension
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'dimensions'
  Prelude.NonEmpty TimestreamDimension ->
  TimestreamAction
newTimestreamAction
  pRoleArn_
  pDatabaseName_
  pTableName_
  pDimensions_ =
    TimestreamAction'
      { timestamp = Prelude.Nothing,
        roleArn = pRoleArn_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        dimensions = Lens.coerced Lens.# pDimensions_
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
timestreamAction_timestamp :: Lens.Lens' TimestreamAction (Prelude.Maybe TimestreamTimestamp)
timestreamAction_timestamp = Lens.lens (\TimestreamAction' {timestamp} -> timestamp) (\s@TimestreamAction' {} a -> s {timestamp = a} :: TimestreamAction)

-- | The ARN of the role that grants permission to write to the Amazon
-- Timestream database table.
timestreamAction_roleArn :: Lens.Lens' TimestreamAction Prelude.Text
timestreamAction_roleArn = Lens.lens (\TimestreamAction' {roleArn} -> roleArn) (\s@TimestreamAction' {} a -> s {roleArn = a} :: TimestreamAction)

-- | The name of an Amazon Timestream database.
timestreamAction_databaseName :: Lens.Lens' TimestreamAction Prelude.Text
timestreamAction_databaseName = Lens.lens (\TimestreamAction' {databaseName} -> databaseName) (\s@TimestreamAction' {} a -> s {databaseName = a} :: TimestreamAction)

-- | The name of the database table into which to write the measure records.
timestreamAction_tableName :: Lens.Lens' TimestreamAction Prelude.Text
timestreamAction_tableName = Lens.lens (\TimestreamAction' {tableName} -> tableName) (\s@TimestreamAction' {} a -> s {tableName = a} :: TimestreamAction)

-- | Metadata attributes of the time series that are written in each measure
-- record.
timestreamAction_dimensions :: Lens.Lens' TimestreamAction (Prelude.NonEmpty TimestreamDimension)
timestreamAction_dimensions = Lens.lens (\TimestreamAction' {dimensions} -> dimensions) (\s@TimestreamAction' {} a -> s {dimensions = a} :: TimestreamAction) Prelude.. Lens.coerced

instance Core.FromJSON TimestreamAction where
  parseJSON =
    Core.withObject
      "TimestreamAction"
      ( \x ->
          TimestreamAction'
            Prelude.<$> (x Core..:? "timestamp")
            Prelude.<*> (x Core..: "roleArn")
            Prelude.<*> (x Core..: "databaseName")
            Prelude.<*> (x Core..: "tableName")
            Prelude.<*> (x Core..: "dimensions")
      )

instance Prelude.Hashable TimestreamAction where
  hashWithSalt _salt TimestreamAction' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` dimensions

instance Prelude.NFData TimestreamAction where
  rnf TimestreamAction' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf dimensions

instance Core.ToJSON TimestreamAction where
  toJSON TimestreamAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("timestamp" Core..=) Prelude.<$> timestamp,
            Prelude.Just ("roleArn" Core..= roleArn),
            Prelude.Just ("databaseName" Core..= databaseName),
            Prelude.Just ("tableName" Core..= tableName),
            Prelude.Just ("dimensions" Core..= dimensions)
          ]
      )
