{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TimestreamAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TimestreamAction
  ( TimestreamAction (..)
  -- * Smart constructor
  , mkTimestreamAction
  -- * Lenses
  , taRoleArn
  , taDatabaseName
  , taTableName
  , taDimensions
  , taTimestamp
  ) where

import qualified Network.AWS.IoT.Types.AwsArn as Types
import qualified Network.AWS.IoT.Types.DatabaseName as Types
import qualified Network.AWS.IoT.Types.TimestreamDimension as Types
import qualified Network.AWS.IoT.Types.TimestreamTableName as Types
import qualified Network.AWS.IoT.Types.TimestreamTimestamp as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Timestream rule action writes attributes (measures) from an MQTT message into an Amazon Timestream table. For more information, see the <https://docs.aws.amazon.com/iot/latest/developerguide/timestream-rule-action.html Timestream> topic rule action documentation.
--
-- /See:/ 'mkTimestreamAction' smart constructor.
data TimestreamAction = TimestreamAction'
  { roleArn :: Types.AwsArn
    -- ^ The ARN of the role that grants permission to write to the Amazon Timestream database table.
  , databaseName :: Types.DatabaseName
    -- ^ The name of an Amazon Timestream database.
  , tableName :: Types.TimestreamTableName
    -- ^ The name of the database table into which to write the measure records.
  , dimensions :: Core.NonEmpty Types.TimestreamDimension
    -- ^ Metadata attributes of the time series that are written in each measure record.
  , timestamp :: Core.Maybe Types.TimestreamTimestamp
    -- ^ Specifies an application-defined value to replace the default value assigned to the Timestream record's timestamp in the @time@ column.
--
-- You can use this property to specify the value and the precision of the Timestream record's timestamp. You can specify a value from the message payload or a value computed by a substitution template.
-- If omitted, the topic rule action assigns the timestamp, in milliseconds, at the time it processed the rule. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TimestreamAction' value with any optional fields omitted.
mkTimestreamAction
    :: Types.AwsArn -- ^ 'roleArn'
    -> Types.DatabaseName -- ^ 'databaseName'
    -> Types.TimestreamTableName -- ^ 'tableName'
    -> Core.NonEmpty Types.TimestreamDimension -- ^ 'dimensions'
    -> TimestreamAction
mkTimestreamAction roleArn databaseName tableName dimensions
  = TimestreamAction'{roleArn, databaseName, tableName, dimensions,
                      timestamp = Core.Nothing}

-- | The ARN of the role that grants permission to write to the Amazon Timestream database table.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taRoleArn :: Lens.Lens' TimestreamAction Types.AwsArn
taRoleArn = Lens.field @"roleArn"
{-# INLINEABLE taRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The name of an Amazon Timestream database.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taDatabaseName :: Lens.Lens' TimestreamAction Types.DatabaseName
taDatabaseName = Lens.field @"databaseName"
{-# INLINEABLE taDatabaseName #-}
{-# DEPRECATED databaseName "Use generic-lens or generic-optics with 'databaseName' instead"  #-}

-- | The name of the database table into which to write the measure records.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTableName :: Lens.Lens' TimestreamAction Types.TimestreamTableName
taTableName = Lens.field @"tableName"
{-# INLINEABLE taTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

-- | Metadata attributes of the time series that are written in each measure record.
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taDimensions :: Lens.Lens' TimestreamAction (Core.NonEmpty Types.TimestreamDimension)
taDimensions = Lens.field @"dimensions"
{-# INLINEABLE taDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

-- | Specifies an application-defined value to replace the default value assigned to the Timestream record's timestamp in the @time@ column.
--
-- You can use this property to specify the value and the precision of the Timestream record's timestamp. You can specify a value from the message payload or a value computed by a substitution template.
-- If omitted, the topic rule action assigns the timestamp, in milliseconds, at the time it processed the rule. 
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTimestamp :: Lens.Lens' TimestreamAction (Core.Maybe Types.TimestreamTimestamp)
taTimestamp = Lens.field @"timestamp"
{-# INLINEABLE taTimestamp #-}
{-# DEPRECATED timestamp "Use generic-lens or generic-optics with 'timestamp' instead"  #-}

instance Core.FromJSON TimestreamAction where
        toJSON TimestreamAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("roleArn" Core..= roleArn),
                  Core.Just ("databaseName" Core..= databaseName),
                  Core.Just ("tableName" Core..= tableName),
                  Core.Just ("dimensions" Core..= dimensions),
                  ("timestamp" Core..=) Core.<$> timestamp])

instance Core.FromJSON TimestreamAction where
        parseJSON
          = Core.withObject "TimestreamAction" Core.$
              \ x ->
                TimestreamAction' Core.<$>
                  (x Core..: "roleArn") Core.<*> x Core..: "databaseName" Core.<*>
                    x Core..: "tableName"
                    Core.<*> x Core..: "dimensions"
                    Core.<*> x Core..:? "timestamp"
