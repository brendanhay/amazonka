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
    taTimestamp,
    taRoleARN,
    taDatabaseName,
    taTableName,
    taDimensions,
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
  { timestamp ::
      Lude.Maybe TimestreamTimestamp,
    roleARN :: Lude.Text,
    databaseName :: Lude.Text,
    tableName :: Lude.Text,
    dimensions :: Lude.NonEmpty TimestreamDimension
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimestreamAction' with the minimum fields required to make a request.
--
-- * 'databaseName' - The name of an Amazon Timestream database.
-- * 'dimensions' - Metadata attributes of the time series that are written in each measure record.
-- * 'roleARN' - The ARN of the role that grants permission to write to the Amazon Timestream database table.
-- * 'tableName' - The name of the database table into which to write the measure records.
-- * 'timestamp' - Specifies an application-defined value to replace the default value assigned to the Timestream record's timestamp in the @time@ column.
--
-- You can use this property to specify the value and the precision of the Timestream record's timestamp. You can specify a value from the message payload or a value computed by a substitution template.
-- If omitted, the topic rule action assigns the timestamp, in milliseconds, at the time it processed the rule.
mkTimestreamAction ::
  -- | 'roleARN'
  Lude.Text ->
  -- | 'databaseName'
  Lude.Text ->
  -- | 'tableName'
  Lude.Text ->
  -- | 'dimensions'
  Lude.NonEmpty TimestreamDimension ->
  TimestreamAction
mkTimestreamAction
  pRoleARN_
  pDatabaseName_
  pTableName_
  pDimensions_ =
    TimestreamAction'
      { timestamp = Lude.Nothing,
        roleARN = pRoleARN_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        dimensions = pDimensions_
      }

-- | Specifies an application-defined value to replace the default value assigned to the Timestream record's timestamp in the @time@ column.
--
-- You can use this property to specify the value and the precision of the Timestream record's timestamp. You can specify a value from the message payload or a value computed by a substitution template.
-- If omitted, the topic rule action assigns the timestamp, in milliseconds, at the time it processed the rule.
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTimestamp :: Lens.Lens' TimestreamAction (Lude.Maybe TimestreamTimestamp)
taTimestamp = Lens.lens (timestamp :: TimestreamAction -> Lude.Maybe TimestreamTimestamp) (\s a -> s {timestamp = a} :: TimestreamAction)
{-# DEPRECATED taTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

-- | The ARN of the role that grants permission to write to the Amazon Timestream database table.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taRoleARN :: Lens.Lens' TimestreamAction Lude.Text
taRoleARN = Lens.lens (roleARN :: TimestreamAction -> Lude.Text) (\s a -> s {roleARN = a} :: TimestreamAction)
{-# DEPRECATED taRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name of an Amazon Timestream database.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taDatabaseName :: Lens.Lens' TimestreamAction Lude.Text
taDatabaseName = Lens.lens (databaseName :: TimestreamAction -> Lude.Text) (\s a -> s {databaseName = a} :: TimestreamAction)
{-# DEPRECATED taDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The name of the database table into which to write the measure records.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTableName :: Lens.Lens' TimestreamAction Lude.Text
taTableName = Lens.lens (tableName :: TimestreamAction -> Lude.Text) (\s a -> s {tableName = a} :: TimestreamAction)
{-# DEPRECATED taTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | Metadata attributes of the time series that are written in each measure record.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taDimensions :: Lens.Lens' TimestreamAction (Lude.NonEmpty TimestreamDimension)
taDimensions = Lens.lens (dimensions :: TimestreamAction -> Lude.NonEmpty TimestreamDimension) (\s a -> s {dimensions = a} :: TimestreamAction)
{-# DEPRECATED taDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

instance Lude.FromJSON TimestreamAction where
  parseJSON =
    Lude.withObject
      "TimestreamAction"
      ( \x ->
          TimestreamAction'
            Lude.<$> (x Lude..:? "timestamp")
            Lude.<*> (x Lude..: "roleArn")
            Lude.<*> (x Lude..: "databaseName")
            Lude.<*> (x Lude..: "tableName")
            Lude.<*> (x Lude..: "dimensions")
      )

instance Lude.ToJSON TimestreamAction where
  toJSON TimestreamAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("timestamp" Lude..=) Lude.<$> timestamp,
            Lude.Just ("roleArn" Lude..= roleARN),
            Lude.Just ("databaseName" Lude..= databaseName),
            Lude.Just ("tableName" Lude..= tableName),
            Lude.Just ("dimensions" Lude..= dimensions)
          ]
      )
