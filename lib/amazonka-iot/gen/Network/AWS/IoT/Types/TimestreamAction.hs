{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimestreamAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TimestreamAction
  ( TimestreamAction (..),

    -- * Smart constructor
    mkTimestreamAction,

    -- * Lenses
    taDatabaseName,
    taDimensions,
    taTimestamp,
    taTableName,
    taRoleARN,
  )
where

import Network.AWS.IoT.Types.TimestreamDimension
import Network.AWS.IoT.Types.TimestreamTimestamp
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Timestream rule action writes attributes (measures) from an MQTT message into an Amazon Timestream table. For more information, see the <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream> topic rule action documentation.
--
-- /See:/ 'mkTimestreamAction' smart constructor.
data TimestreamAction = TimestreamAction'
  { -- | The name of an Amazon Timestream database.
    databaseName :: Lude.Text,
    -- | Metadata attributes of the time series that are written in each measure record.
    dimensions :: Lude.NonEmpty TimestreamDimension,
    -- | Specifies an application-defined value to replace the default value assigned to the Timestream record's timestamp in the @time@ column.
    --
    -- You can use this property to specify the value and the precision of the Timestream record's timestamp. You can specify a value from the message payload or a value computed by a substitution template.
    -- If omitted, the topic rule action assigns the timestamp, in milliseconds, at the time it processed the rule.
    timestamp :: Lude.Maybe TimestreamTimestamp,
    -- | The name of the database table into which to write the measure records.
    tableName :: Lude.Text,
    -- | The ARN of the role that grants permission to write to the Amazon Timestream database table.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimestreamAction' with the minimum fields required to make a request.
--
-- * 'databaseName' - The name of an Amazon Timestream database.
-- * 'dimensions' - Metadata attributes of the time series that are written in each measure record.
-- * 'timestamp' - Specifies an application-defined value to replace the default value assigned to the Timestream record's timestamp in the @time@ column.
--
-- You can use this property to specify the value and the precision of the Timestream record's timestamp. You can specify a value from the message payload or a value computed by a substitution template.
-- If omitted, the topic rule action assigns the timestamp, in milliseconds, at the time it processed the rule.
-- * 'tableName' - The name of the database table into which to write the measure records.
-- * 'roleARN' - The ARN of the role that grants permission to write to the Amazon Timestream database table.
mkTimestreamAction ::
  -- | 'databaseName'
  Lude.Text ->
  -- | 'dimensions'
  Lude.NonEmpty TimestreamDimension ->
  -- | 'tableName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  TimestreamAction
mkTimestreamAction
  pDatabaseName_
  pDimensions_
  pTableName_
  pRoleARN_ =
    TimestreamAction'
      { databaseName = pDatabaseName_,
        dimensions = pDimensions_,
        timestamp = Lude.Nothing,
        tableName = pTableName_,
        roleARN = pRoleARN_
      }

-- | The name of an Amazon Timestream database.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taDatabaseName :: Lens.Lens' TimestreamAction Lude.Text
taDatabaseName = Lens.lens (databaseName :: TimestreamAction -> Lude.Text) (\s a -> s {databaseName = a} :: TimestreamAction)
{-# DEPRECATED taDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Metadata attributes of the time series that are written in each measure record.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taDimensions :: Lens.Lens' TimestreamAction (Lude.NonEmpty TimestreamDimension)
taDimensions = Lens.lens (dimensions :: TimestreamAction -> Lude.NonEmpty TimestreamDimension) (\s a -> s {dimensions = a} :: TimestreamAction)
{-# DEPRECATED taDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | Specifies an application-defined value to replace the default value assigned to the Timestream record's timestamp in the @time@ column.
--
-- You can use this property to specify the value and the precision of the Timestream record's timestamp. You can specify a value from the message payload or a value computed by a substitution template.
-- If omitted, the topic rule action assigns the timestamp, in milliseconds, at the time it processed the rule.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTimestamp :: Lens.Lens' TimestreamAction (Lude.Maybe TimestreamTimestamp)
taTimestamp = Lens.lens (timestamp :: TimestreamAction -> Lude.Maybe TimestreamTimestamp) (\s a -> s {timestamp = a} :: TimestreamAction)
{-# DEPRECATED taTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The name of the database table into which to write the measure records.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTableName :: Lens.Lens' TimestreamAction Lude.Text
taTableName = Lens.lens (tableName :: TimestreamAction -> Lude.Text) (\s a -> s {tableName = a} :: TimestreamAction)
{-# DEPRECATED taTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The ARN of the role that grants permission to write to the Amazon Timestream database table.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taRoleARN :: Lens.Lens' TimestreamAction Lude.Text
taRoleARN = Lens.lens (roleARN :: TimestreamAction -> Lude.Text) (\s a -> s {roleARN = a} :: TimestreamAction)
{-# DEPRECATED taRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON TimestreamAction where
  parseJSON =
    Lude.withObject
      "TimestreamAction"
      ( \x ->
          TimestreamAction'
            Lude.<$> (x Lude..: "databaseName")
            Lude.<*> (x Lude..: "dimensions")
            Lude.<*> (x Lude..:? "timestamp")
            Lude.<*> (x Lude..: "tableName")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON TimestreamAction where
  toJSON TimestreamAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("databaseName" Lude..= databaseName),
            Lude.Just ("dimensions" Lude..= dimensions),
            ("timestamp" Lude..=) Lude.<$> timestamp,
            Lude.Just ("tableName" Lude..= tableName),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
