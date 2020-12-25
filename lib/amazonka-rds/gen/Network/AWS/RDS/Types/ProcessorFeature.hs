{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ProcessorFeature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ProcessorFeature
  ( ProcessorFeature (..),

    -- * Smart constructor
    mkProcessorFeature,

    -- * Lenses
    pfName,
    pfValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.String as Types

-- | Contains the processor features of a DB instance class.
--
-- To specify the number of CPU cores, use the @coreCount@ feature name for the @Name@ parameter. To specify the number of threads per core, use the @threadsPerCore@ feature name for the @Name@ parameter.
-- You can set the processor features of the DB instance class for a DB instance when you call one of the following actions:
--
--     * @CreateDBInstance@
--
--
--     * @ModifyDBInstance@
--
--
--     * @RestoreDBInstanceFromDBSnapshot@
--
--
--     * @RestoreDBInstanceFromS3@
--
--
--     * @RestoreDBInstanceToPointInTime@
--
--
-- You can view the valid processor values for a particular instance class by calling the @DescribeOrderableDBInstanceOptions@ action and specifying the instance class for the @DBInstanceClass@ parameter.
-- In addition, you can use the following actions for DB instance class processor information:
--
--     * @DescribeDBInstances@
--
--
--     * @DescribeDBSnapshots@
--
--
--     * @DescribeValidDBInstanceModifications@
--
--
-- If you call @DescribeDBInstances@ , @ProcessorFeature@ returns non-null values only if the following conditions are met:
--
--     * You are accessing an Oracle DB instance.
--
--
--     * Your Oracle DB instance class supports configuring the number of CPU cores and threads per core.
--
--
--     * The current number CPU cores and threads is set to a non-default value.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html#USER_ConfigureProcessor Configuring the Processor of the DB Instance Class> in the /Amazon RDS User Guide. /
--
-- /See:/ 'mkProcessorFeature' smart constructor.
data ProcessorFeature = ProcessorFeature'
  { -- | The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
    name :: Core.Maybe Types.String,
    -- | The value of a processor feature name.
    value :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ProcessorFeature' value with any optional fields omitted.
mkProcessorFeature ::
  ProcessorFeature
mkProcessorFeature =
  ProcessorFeature' {name = Core.Nothing, value = Core.Nothing}

-- | The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfName :: Lens.Lens' ProcessorFeature (Core.Maybe Types.String)
pfName = Lens.field @"name"
{-# DEPRECATED pfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of a processor feature name.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfValue :: Lens.Lens' ProcessorFeature (Core.Maybe Types.String)
pfValue = Lens.field @"value"
{-# DEPRECATED pfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML ProcessorFeature where
  parseXML x =
    ProcessorFeature'
      Core.<$> (x Core..@? "Name") Core.<*> (x Core..@? "Value")
