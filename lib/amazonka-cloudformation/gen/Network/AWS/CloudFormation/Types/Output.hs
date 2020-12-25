{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Output
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Output
  ( Output (..),

    -- * Smart constructor
    mkOutput,

    -- * Lenses
    oDescription,
    oExportName,
    oOutputKey,
    oOutputValue,
  )
where

import qualified Network.AWS.CloudFormation.Types.Description as Types
import qualified Network.AWS.CloudFormation.Types.ExportName as Types
import qualified Network.AWS.CloudFormation.Types.OutputKey as Types
import qualified Network.AWS.CloudFormation.Types.OutputValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Output data type.
--
-- /See:/ 'mkOutput' smart constructor.
data Output = Output'
  { -- | User defined description associated with the output.
    description :: Core.Maybe Types.Description,
    -- | The name of the export associated with the output.
    exportName :: Core.Maybe Types.ExportName,
    -- | The key associated with the output.
    outputKey :: Core.Maybe Types.OutputKey,
    -- | The value associated with the output.
    outputValue :: Core.Maybe Types.OutputValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Output' value with any optional fields omitted.
mkOutput ::
  Output
mkOutput =
  Output'
    { description = Core.Nothing,
      exportName = Core.Nothing,
      outputKey = Core.Nothing,
      outputValue = Core.Nothing
    }

-- | User defined description associated with the output.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oDescription :: Lens.Lens' Output (Core.Maybe Types.Description)
oDescription = Lens.field @"description"
{-# DEPRECATED oDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the export associated with the output.
--
-- /Note:/ Consider using 'exportName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oExportName :: Lens.Lens' Output (Core.Maybe Types.ExportName)
oExportName = Lens.field @"exportName"
{-# DEPRECATED oExportName "Use generic-lens or generic-optics with 'exportName' instead." #-}

-- | The key associated with the output.
--
-- /Note:/ Consider using 'outputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOutputKey :: Lens.Lens' Output (Core.Maybe Types.OutputKey)
oOutputKey = Lens.field @"outputKey"
{-# DEPRECATED oOutputKey "Use generic-lens or generic-optics with 'outputKey' instead." #-}

-- | The value associated with the output.
--
-- /Note:/ Consider using 'outputValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOutputValue :: Lens.Lens' Output (Core.Maybe Types.OutputValue)
oOutputValue = Lens.field @"outputValue"
{-# DEPRECATED oOutputValue "Use generic-lens or generic-optics with 'outputValue' instead." #-}

instance Core.FromXML Output where
  parseXML x =
    Output'
      Core.<$> (x Core..@? "Description")
      Core.<*> (x Core..@? "ExportName")
      Core.<*> (x Core..@? "OutputKey")
      Core.<*> (x Core..@? "OutputValue")
