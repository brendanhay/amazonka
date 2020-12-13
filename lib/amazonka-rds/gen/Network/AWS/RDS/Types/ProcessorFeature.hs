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
    pfValue,
    pfName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | The value of a processor feature name.
    value :: Lude.Maybe Lude.Text,
    -- | The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProcessorFeature' with the minimum fields required to make a request.
--
-- * 'value' - The value of a processor feature name.
-- * 'name' - The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
mkProcessorFeature ::
  ProcessorFeature
mkProcessorFeature =
  ProcessorFeature' {value = Lude.Nothing, name = Lude.Nothing}

-- | The value of a processor feature name.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfValue :: Lens.Lens' ProcessorFeature (Lude.Maybe Lude.Text)
pfValue = Lens.lens (value :: ProcessorFeature -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ProcessorFeature)
{-# DEPRECATED pfValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the processor feature. Valid names are @coreCount@ and @threadsPerCore@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pfName :: Lens.Lens' ProcessorFeature (Lude.Maybe Lude.Text)
pfName = Lens.lens (name :: ProcessorFeature -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProcessorFeature)
{-# DEPRECATED pfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML ProcessorFeature where
  parseXML x =
    ProcessorFeature'
      Lude.<$> (x Lude..@? "Value") Lude.<*> (x Lude..@? "Name")

instance Lude.ToQuery ProcessorFeature where
  toQuery ProcessorFeature' {..} =
    Lude.mconcat ["Value" Lude.=: value, "Name" Lude.=: name]
