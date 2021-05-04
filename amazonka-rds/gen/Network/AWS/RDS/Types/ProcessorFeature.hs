{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.RDS.Types.ProcessorFeature
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ProcessorFeature where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the processor features of a DB instance class.
--
-- To specify the number of CPU cores, use the @coreCount@ feature name for
-- the @Name@ parameter. To specify the number of threads per core, use the
-- @threadsPerCore@ feature name for the @Name@ parameter.
--
-- You can set the processor features of the DB instance class for a DB
-- instance when you call one of the following actions:
--
-- -   @CreateDBInstance@
--
-- -   @ModifyDBInstance@
--
-- -   @RestoreDBInstanceFromDBSnapshot@
--
-- -   @RestoreDBInstanceFromS3@
--
-- -   @RestoreDBInstanceToPointInTime@
--
-- You can view the valid processor values for a particular instance class
-- by calling the @DescribeOrderableDBInstanceOptions@ action and
-- specifying the instance class for the @DBInstanceClass@ parameter.
--
-- In addition, you can use the following actions for DB instance class
-- processor information:
--
-- -   @DescribeDBInstances@
--
-- -   @DescribeDBSnapshots@
--
-- -   @DescribeValidDBInstanceModifications@
--
-- If you call @DescribeDBInstances@, @ProcessorFeature@ returns non-null
-- values only if the following conditions are met:
--
-- -   You are accessing an Oracle DB instance.
--
-- -   Your Oracle DB instance class supports configuring the number of CPU
--     cores and threads per core.
--
-- -   The current number CPU cores and threads is set to a non-default
--     value.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html#USER_ConfigureProcessor Configuring the Processor of the DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- /See:/ 'newProcessorFeature' smart constructor.
data ProcessorFeature = ProcessorFeature'
  { -- | The name of the processor feature. Valid names are @coreCount@ and
    -- @threadsPerCore@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of a processor feature name.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProcessorFeature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'processorFeature_name' - The name of the processor feature. Valid names are @coreCount@ and
-- @threadsPerCore@.
--
-- 'value', 'processorFeature_value' - The value of a processor feature name.
newProcessorFeature ::
  ProcessorFeature
newProcessorFeature =
  ProcessorFeature'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the processor feature. Valid names are @coreCount@ and
-- @threadsPerCore@.
processorFeature_name :: Lens.Lens' ProcessorFeature (Prelude.Maybe Prelude.Text)
processorFeature_name = Lens.lens (\ProcessorFeature' {name} -> name) (\s@ProcessorFeature' {} a -> s {name = a} :: ProcessorFeature)

-- | The value of a processor feature name.
processorFeature_value :: Lens.Lens' ProcessorFeature (Prelude.Maybe Prelude.Text)
processorFeature_value = Lens.lens (\ProcessorFeature' {value} -> value) (\s@ProcessorFeature' {} a -> s {value = a} :: ProcessorFeature)

instance Prelude.FromXML ProcessorFeature where
  parseXML x =
    ProcessorFeature'
      Prelude.<$> (x Prelude..@? "Name")
      Prelude.<*> (x Prelude..@? "Value")

instance Prelude.Hashable ProcessorFeature

instance Prelude.NFData ProcessorFeature

instance Prelude.ToQuery ProcessorFeature where
  toQuery ProcessorFeature' {..} =
    Prelude.mconcat
      ["Name" Prelude.=: name, "Value" Prelude.=: value]
