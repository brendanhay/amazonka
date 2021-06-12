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
-- Module      : Network.AWS.EC2.Types.ProcessorInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ProcessorInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ArchitectureType
import qualified Network.AWS.Lens as Lens

-- | Describes the processor used by the instance type.
--
-- /See:/ 'newProcessorInfo' smart constructor.
data ProcessorInfo = ProcessorInfo'
  { -- | The speed of the processor, in GHz.
    sustainedClockSpeedInGhz :: Core.Maybe Core.Double,
    -- | The architectures supported by the instance type.
    supportedArchitectures :: Core.Maybe [ArchitectureType]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProcessorInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sustainedClockSpeedInGhz', 'processorInfo_sustainedClockSpeedInGhz' - The speed of the processor, in GHz.
--
-- 'supportedArchitectures', 'processorInfo_supportedArchitectures' - The architectures supported by the instance type.
newProcessorInfo ::
  ProcessorInfo
newProcessorInfo =
  ProcessorInfo'
    { sustainedClockSpeedInGhz =
        Core.Nothing,
      supportedArchitectures = Core.Nothing
    }

-- | The speed of the processor, in GHz.
processorInfo_sustainedClockSpeedInGhz :: Lens.Lens' ProcessorInfo (Core.Maybe Core.Double)
processorInfo_sustainedClockSpeedInGhz = Lens.lens (\ProcessorInfo' {sustainedClockSpeedInGhz} -> sustainedClockSpeedInGhz) (\s@ProcessorInfo' {} a -> s {sustainedClockSpeedInGhz = a} :: ProcessorInfo)

-- | The architectures supported by the instance type.
processorInfo_supportedArchitectures :: Lens.Lens' ProcessorInfo (Core.Maybe [ArchitectureType])
processorInfo_supportedArchitectures = Lens.lens (\ProcessorInfo' {supportedArchitectures} -> supportedArchitectures) (\s@ProcessorInfo' {} a -> s {supportedArchitectures = a} :: ProcessorInfo) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML ProcessorInfo where
  parseXML x =
    ProcessorInfo'
      Core.<$> (x Core..@? "sustainedClockSpeedInGhz")
      Core.<*> ( x Core..@? "supportedArchitectures"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )

instance Core.Hashable ProcessorInfo

instance Core.NFData ProcessorInfo
