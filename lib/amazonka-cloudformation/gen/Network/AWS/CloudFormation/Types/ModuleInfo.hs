{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ModuleInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ModuleInfo
  ( ModuleInfo (..),

    -- * Smart constructor
    mkModuleInfo,

    -- * Lenses
    miLogicalIdHierarchy,
    miTypeHierarchy,
  )
where

import qualified Network.AWS.CloudFormation.Types.LogicalIdHierarchy as Types
import qualified Network.AWS.CloudFormation.Types.TypeHierarchy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- For more information on modules, see <AWSCloudFormation/latest/UserGuide/modules.html Using modules to encapsulate and reuse resource configurations> in the /CloudFormation User Guide/ .
--
-- /See:/ 'mkModuleInfo' smart constructor.
data ModuleInfo = ModuleInfo'
  { -- | A concantenated list of the logical IDs of the module or modules containing the resource. Modules are listed starting with the inner-most nested module, and separated by @/@ .
    --
    -- In the following example, the resource was created from a module, @moduleA@ , that is nested inside a parent module, @moduleB@ .
    -- @moduleA/moduleB@
    -- For more information, see <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module> in the /CloudFormation User Guide/ .
    logicalIdHierarchy :: Core.Maybe Types.LogicalIdHierarchy,
    -- | A concantenated list of the the module type or types containing the resource. Module types are listed starting with the inner-most nested module, and separated by @/@ .
    --
    -- In the following example, the resource was created from a module of type @AWS::First::Example::MODULE@ , that is nested inside a parent module of type @AWS::Second::Example::MODULE@ .
    -- @AWS::First::Example::MODULE/AWS::Second::Example::MODULE@
    typeHierarchy :: Core.Maybe Types.TypeHierarchy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModuleInfo' value with any optional fields omitted.
mkModuleInfo ::
  ModuleInfo
mkModuleInfo =
  ModuleInfo'
    { logicalIdHierarchy = Core.Nothing,
      typeHierarchy = Core.Nothing
    }

-- | A concantenated list of the logical IDs of the module or modules containing the resource. Modules are listed starting with the inner-most nested module, and separated by @/@ .
--
-- In the following example, the resource was created from a module, @moduleA@ , that is nested inside a parent module, @moduleB@ .
-- @moduleA/moduleB@
-- For more information, see <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module> in the /CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'logicalIdHierarchy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miLogicalIdHierarchy :: Lens.Lens' ModuleInfo (Core.Maybe Types.LogicalIdHierarchy)
miLogicalIdHierarchy = Lens.field @"logicalIdHierarchy"
{-# DEPRECATED miLogicalIdHierarchy "Use generic-lens or generic-optics with 'logicalIdHierarchy' instead." #-}

-- | A concantenated list of the the module type or types containing the resource. Module types are listed starting with the inner-most nested module, and separated by @/@ .
--
-- In the following example, the resource was created from a module of type @AWS::First::Example::MODULE@ , that is nested inside a parent module of type @AWS::Second::Example::MODULE@ .
-- @AWS::First::Example::MODULE/AWS::Second::Example::MODULE@
--
-- /Note:/ Consider using 'typeHierarchy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miTypeHierarchy :: Lens.Lens' ModuleInfo (Core.Maybe Types.TypeHierarchy)
miTypeHierarchy = Lens.field @"typeHierarchy"
{-# DEPRECATED miTypeHierarchy "Use generic-lens or generic-optics with 'typeHierarchy' instead." #-}

instance Core.FromXML ModuleInfo where
  parseXML x =
    ModuleInfo'
      Core.<$> (x Core..@? "LogicalIdHierarchy")
      Core.<*> (x Core..@? "TypeHierarchy")
