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
-- Module      : Network.AWS.CloudFormation.Types.ModuleInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ModuleInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
--
-- For more information on modules, see
-- <AWSCloudFormation/latest/UserGuide/modules.html Using modules to encapsulate and reuse resource configurations>
-- in the /CloudFormation User Guide/.
--
-- /See:/ 'newModuleInfo' smart constructor.
data ModuleInfo = ModuleInfo'
  { -- | A concantenated list of the logical IDs of the module or modules
    -- containing the resource. Modules are listed starting with the inner-most
    -- nested module, and separated by @\/@.
    --
    -- In the following example, the resource was created from a module,
    -- @moduleA@, that is nested inside a parent module, @moduleB@.
    --
    -- @moduleA\/moduleB@
    --
    -- For more information, see
    -- <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module>
    -- in the /CloudFormation User Guide/.
    logicalIdHierarchy :: Prelude.Maybe Prelude.Text,
    -- | A concantenated list of the the module type or types containing the
    -- resource. Module types are listed starting with the inner-most nested
    -- module, and separated by @\/@.
    --
    -- In the following example, the resource was created from a module of type
    -- @AWS::First::Example::MODULE@, that is nested inside a parent module of
    -- type @AWS::Second::Example::MODULE@.
    --
    -- @AWS::First::Example::MODULE\/AWS::Second::Example::MODULE@
    typeHierarchy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModuleInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logicalIdHierarchy', 'moduleInfo_logicalIdHierarchy' - A concantenated list of the logical IDs of the module or modules
-- containing the resource. Modules are listed starting with the inner-most
-- nested module, and separated by @\/@.
--
-- In the following example, the resource was created from a module,
-- @moduleA@, that is nested inside a parent module, @moduleB@.
--
-- @moduleA\/moduleB@
--
-- For more information, see
-- <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module>
-- in the /CloudFormation User Guide/.
--
-- 'typeHierarchy', 'moduleInfo_typeHierarchy' - A concantenated list of the the module type or types containing the
-- resource. Module types are listed starting with the inner-most nested
-- module, and separated by @\/@.
--
-- In the following example, the resource was created from a module of type
-- @AWS::First::Example::MODULE@, that is nested inside a parent module of
-- type @AWS::Second::Example::MODULE@.
--
-- @AWS::First::Example::MODULE\/AWS::Second::Example::MODULE@
newModuleInfo ::
  ModuleInfo
newModuleInfo =
  ModuleInfo'
    { logicalIdHierarchy = Prelude.Nothing,
      typeHierarchy = Prelude.Nothing
    }

-- | A concantenated list of the logical IDs of the module or modules
-- containing the resource. Modules are listed starting with the inner-most
-- nested module, and separated by @\/@.
--
-- In the following example, the resource was created from a module,
-- @moduleA@, that is nested inside a parent module, @moduleB@.
--
-- @moduleA\/moduleB@
--
-- For more information, see
-- <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module>
-- in the /CloudFormation User Guide/.
moduleInfo_logicalIdHierarchy :: Lens.Lens' ModuleInfo (Prelude.Maybe Prelude.Text)
moduleInfo_logicalIdHierarchy = Lens.lens (\ModuleInfo' {logicalIdHierarchy} -> logicalIdHierarchy) (\s@ModuleInfo' {} a -> s {logicalIdHierarchy = a} :: ModuleInfo)

-- | A concantenated list of the the module type or types containing the
-- resource. Module types are listed starting with the inner-most nested
-- module, and separated by @\/@.
--
-- In the following example, the resource was created from a module of type
-- @AWS::First::Example::MODULE@, that is nested inside a parent module of
-- type @AWS::Second::Example::MODULE@.
--
-- @AWS::First::Example::MODULE\/AWS::Second::Example::MODULE@
moduleInfo_typeHierarchy :: Lens.Lens' ModuleInfo (Prelude.Maybe Prelude.Text)
moduleInfo_typeHierarchy = Lens.lens (\ModuleInfo' {typeHierarchy} -> typeHierarchy) (\s@ModuleInfo' {} a -> s {typeHierarchy = a} :: ModuleInfo)

instance Prelude.FromXML ModuleInfo where
  parseXML x =
    ModuleInfo'
      Prelude.<$> (x Prelude..@? "LogicalIdHierarchy")
      Prelude.<*> (x Prelude..@? "TypeHierarchy")

instance Prelude.Hashable ModuleInfo

instance Prelude.NFData ModuleInfo
