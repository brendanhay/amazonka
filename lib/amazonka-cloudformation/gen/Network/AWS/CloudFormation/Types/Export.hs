{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.Export
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.Export
  ( Export (..)
  -- * Smart constructor
  , mkExport
  -- * Lenses
  , eExportingStackId
  , eName
  , eValue
  ) where

import qualified Network.AWS.CloudFormation.Types.ExportingStackId as Types
import qualified Network.AWS.CloudFormation.Types.Name as Types
import qualified Network.AWS.CloudFormation.Types.Value as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @Export@ structure describes the exported output values for a stack.
--
-- /See:/ 'mkExport' smart constructor.
data Export = Export'
  { exportingStackId :: Core.Maybe Types.ExportingStackId
    -- ^ The stack that contains the exported output name and value.
  , name :: Core.Maybe Types.Name
    -- ^ The name of exported output value. Use this name and the @Fn::ImportValue@ function to import the associated value into other stacks. The name is defined in the @Export@ field in the associated stack's @Outputs@ section.
  , value :: Core.Maybe Types.Value
    -- ^ The value of the exported output, such as a resource physical ID. This value is defined in the @Export@ field in the associated stack's @Outputs@ section.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Export' value with any optional fields omitted.
mkExport
    :: Export
mkExport
  = Export'{exportingStackId = Core.Nothing, name = Core.Nothing,
            value = Core.Nothing}

-- | The stack that contains the exported output name and value.
--
-- /Note:/ Consider using 'exportingStackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eExportingStackId :: Lens.Lens' Export (Core.Maybe Types.ExportingStackId)
eExportingStackId = Lens.field @"exportingStackId"
{-# INLINEABLE eExportingStackId #-}
{-# DEPRECATED exportingStackId "Use generic-lens or generic-optics with 'exportingStackId' instead"  #-}

-- | The name of exported output value. Use this name and the @Fn::ImportValue@ function to import the associated value into other stacks. The name is defined in the @Export@ field in the associated stack's @Outputs@ section.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eName :: Lens.Lens' Export (Core.Maybe Types.Name)
eName = Lens.field @"name"
{-# INLINEABLE eName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value of the exported output, such as a resource physical ID. This value is defined in the @Export@ field in the associated stack's @Outputs@ section.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eValue :: Lens.Lens' Export (Core.Maybe Types.Value)
eValue = Lens.field @"value"
{-# INLINEABLE eValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromXML Export where
        parseXML x
          = Export' Core.<$>
              (x Core..@? "ExportingStackId") Core.<*> x Core..@? "Name" Core.<*>
                x Core..@? "Value"
