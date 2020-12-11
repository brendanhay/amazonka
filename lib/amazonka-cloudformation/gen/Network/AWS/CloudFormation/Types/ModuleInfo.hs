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
    miTypeHierarchy,
    miLogicalIdHierarchy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the module from which the resource was created, if the resource was created from a module included in the stack template.
--
-- For more information on modules, see <AWSCloudFormation/latest/UserGuide/modules.html Using modules to encapsulate and reuse resource configurations> in the /CloudFormation User Guide/ .
--
-- /See:/ 'mkModuleInfo' smart constructor.
data ModuleInfo = ModuleInfo'
  { typeHierarchy ::
      Lude.Maybe Lude.Text,
    logicalIdHierarchy :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModuleInfo' with the minimum fields required to make a request.
--
-- * 'logicalIdHierarchy' - A concantenated list of the logical IDs of the module or modules containing the resource. Modules are listed starting with the inner-most nested module, and separated by @/@ .
--
-- In the following example, the resource was created from a module, @moduleA@ , that is nested inside a parent module, @moduleB@ .
-- @moduleA/moduleB@
-- For more information, see <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module> in the /CloudFormation User Guide/ .
-- * 'typeHierarchy' - A concantenated list of the the module type or types containing the resource. Module types are listed starting with the inner-most nested module, and separated by @/@ .
--
-- In the following example, the resource was created from a module of type @AWS::First::Example::MODULE@ , that is nested inside a parent module of type @AWS::Second::Example::MODULE@ .
-- @AWS::First::Example::MODULE/AWS::Second::Example::MODULE@
mkModuleInfo ::
  ModuleInfo
mkModuleInfo =
  ModuleInfo'
    { typeHierarchy = Lude.Nothing,
      logicalIdHierarchy = Lude.Nothing
    }

-- | A concantenated list of the the module type or types containing the resource. Module types are listed starting with the inner-most nested module, and separated by @/@ .
--
-- In the following example, the resource was created from a module of type @AWS::First::Example::MODULE@ , that is nested inside a parent module of type @AWS::Second::Example::MODULE@ .
-- @AWS::First::Example::MODULE/AWS::Second::Example::MODULE@
--
-- /Note:/ Consider using 'typeHierarchy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miTypeHierarchy :: Lens.Lens' ModuleInfo (Lude.Maybe Lude.Text)
miTypeHierarchy = Lens.lens (typeHierarchy :: ModuleInfo -> Lude.Maybe Lude.Text) (\s a -> s {typeHierarchy = a} :: ModuleInfo)
{-# DEPRECATED miTypeHierarchy "Use generic-lens or generic-optics with 'typeHierarchy' instead." #-}

-- | A concantenated list of the logical IDs of the module or modules containing the resource. Modules are listed starting with the inner-most nested module, and separated by @/@ .
--
-- In the following example, the resource was created from a module, @moduleA@ , that is nested inside a parent module, @moduleB@ .
-- @moduleA/moduleB@
-- For more information, see <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module> in the /CloudFormation User Guide/ .
--
-- /Note:/ Consider using 'logicalIdHierarchy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miLogicalIdHierarchy :: Lens.Lens' ModuleInfo (Lude.Maybe Lude.Text)
miLogicalIdHierarchy = Lens.lens (logicalIdHierarchy :: ModuleInfo -> Lude.Maybe Lude.Text) (\s a -> s {logicalIdHierarchy = a} :: ModuleInfo)
{-# DEPRECATED miLogicalIdHierarchy "Use generic-lens or generic-optics with 'logicalIdHierarchy' instead." #-}

instance Lude.FromXML ModuleInfo where
  parseXML x =
    ModuleInfo'
      Lude.<$> (x Lude..@? "TypeHierarchy")
      Lude.<*> (x Lude..@? "LogicalIdHierarchy")
