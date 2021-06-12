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
-- Module      : Network.AWS.RDS.Types.AvailableProcessorFeature
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.AvailableProcessorFeature where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the available processor feature information for the DB instance
-- class of a DB instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html#USER_ConfigureProcessor Configuring the Processor of the DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- /See:/ 'newAvailableProcessorFeature' smart constructor.
data AvailableProcessorFeature = AvailableProcessorFeature'
  { -- | The allowed values for the processor feature of the DB instance class.
    allowedValues :: Core.Maybe Core.Text,
    -- | The name of the processor feature. Valid names are @coreCount@ and
    -- @threadsPerCore@.
    name :: Core.Maybe Core.Text,
    -- | The default value for the processor feature of the DB instance class.
    defaultValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AvailableProcessorFeature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'availableProcessorFeature_allowedValues' - The allowed values for the processor feature of the DB instance class.
--
-- 'name', 'availableProcessorFeature_name' - The name of the processor feature. Valid names are @coreCount@ and
-- @threadsPerCore@.
--
-- 'defaultValue', 'availableProcessorFeature_defaultValue' - The default value for the processor feature of the DB instance class.
newAvailableProcessorFeature ::
  AvailableProcessorFeature
newAvailableProcessorFeature =
  AvailableProcessorFeature'
    { allowedValues =
        Core.Nothing,
      name = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | The allowed values for the processor feature of the DB instance class.
availableProcessorFeature_allowedValues :: Lens.Lens' AvailableProcessorFeature (Core.Maybe Core.Text)
availableProcessorFeature_allowedValues = Lens.lens (\AvailableProcessorFeature' {allowedValues} -> allowedValues) (\s@AvailableProcessorFeature' {} a -> s {allowedValues = a} :: AvailableProcessorFeature)

-- | The name of the processor feature. Valid names are @coreCount@ and
-- @threadsPerCore@.
availableProcessorFeature_name :: Lens.Lens' AvailableProcessorFeature (Core.Maybe Core.Text)
availableProcessorFeature_name = Lens.lens (\AvailableProcessorFeature' {name} -> name) (\s@AvailableProcessorFeature' {} a -> s {name = a} :: AvailableProcessorFeature)

-- | The default value for the processor feature of the DB instance class.
availableProcessorFeature_defaultValue :: Lens.Lens' AvailableProcessorFeature (Core.Maybe Core.Text)
availableProcessorFeature_defaultValue = Lens.lens (\AvailableProcessorFeature' {defaultValue} -> defaultValue) (\s@AvailableProcessorFeature' {} a -> s {defaultValue = a} :: AvailableProcessorFeature)

instance Core.FromXML AvailableProcessorFeature where
  parseXML x =
    AvailableProcessorFeature'
      Core.<$> (x Core..@? "AllowedValues")
      Core.<*> (x Core..@? "Name")
      Core.<*> (x Core..@? "DefaultValue")

instance Core.Hashable AvailableProcessorFeature

instance Core.NFData AvailableProcessorFeature
