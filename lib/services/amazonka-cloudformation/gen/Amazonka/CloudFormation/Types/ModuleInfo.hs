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
-- Module      : Amazonka.CloudFormation.Types.ModuleInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ModuleInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the module from which the resource was
-- created, if the resource was created from a module included in the stack
-- template.
--
-- For more information about modules, see
-- <AWSCloudFormation/latest/UserGuide/modules.html Using modules to encapsulate and reuse resource configurations>
-- in the /CloudFormation User Guide/.
--
-- /See:/ 'newModuleInfo' smart constructor.
data ModuleInfo = ModuleInfo'
  { -- | A concatenated list of the logical IDs of the module or modules
    -- containing the resource. Modules are listed starting with the inner-most
    -- nested module, and separated by @\/@.
    --
    -- In the following example, the resource was created from a module,
    -- @moduleA@, that\'s nested inside a parent module, @moduleB@.
    --
    -- @moduleA\/moduleB@
    --
    -- For more information, see
    -- <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module>
    -- in the /CloudFormation User Guide/.
    logicalIdHierarchy :: Prelude.Maybe Prelude.Text,
    -- | A concatenated list of the module type or types containing the resource.
    -- Module types are listed starting with the inner-most nested module, and
    -- separated by @\/@.
    --
    -- In the following example, the resource was created from a module of type
    -- @AWS::First::Example::MODULE@, that\'s nested inside a parent module of
    -- type @AWS::Second::Example::MODULE@.
    --
    -- @AWS::First::Example::MODULE\/AWS::Second::Example::MODULE@
    typeHierarchy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModuleInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logicalIdHierarchy', 'moduleInfo_logicalIdHierarchy' - A concatenated list of the logical IDs of the module or modules
-- containing the resource. Modules are listed starting with the inner-most
-- nested module, and separated by @\/@.
--
-- In the following example, the resource was created from a module,
-- @moduleA@, that\'s nested inside a parent module, @moduleB@.
--
-- @moduleA\/moduleB@
--
-- For more information, see
-- <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module>
-- in the /CloudFormation User Guide/.
--
-- 'typeHierarchy', 'moduleInfo_typeHierarchy' - A concatenated list of the module type or types containing the resource.
-- Module types are listed starting with the inner-most nested module, and
-- separated by @\/@.
--
-- In the following example, the resource was created from a module of type
-- @AWS::First::Example::MODULE@, that\'s nested inside a parent module of
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

-- | A concatenated list of the logical IDs of the module or modules
-- containing the resource. Modules are listed starting with the inner-most
-- nested module, and separated by @\/@.
--
-- In the following example, the resource was created from a module,
-- @moduleA@, that\'s nested inside a parent module, @moduleB@.
--
-- @moduleA\/moduleB@
--
-- For more information, see
-- <AWSCloudFormation/latest/UserGuide/modules.html#module-ref-resources Referencing resources in a module>
-- in the /CloudFormation User Guide/.
moduleInfo_logicalIdHierarchy :: Lens.Lens' ModuleInfo (Prelude.Maybe Prelude.Text)
moduleInfo_logicalIdHierarchy = Lens.lens (\ModuleInfo' {logicalIdHierarchy} -> logicalIdHierarchy) (\s@ModuleInfo' {} a -> s {logicalIdHierarchy = a} :: ModuleInfo)

-- | A concatenated list of the module type or types containing the resource.
-- Module types are listed starting with the inner-most nested module, and
-- separated by @\/@.
--
-- In the following example, the resource was created from a module of type
-- @AWS::First::Example::MODULE@, that\'s nested inside a parent module of
-- type @AWS::Second::Example::MODULE@.
--
-- @AWS::First::Example::MODULE\/AWS::Second::Example::MODULE@
moduleInfo_typeHierarchy :: Lens.Lens' ModuleInfo (Prelude.Maybe Prelude.Text)
moduleInfo_typeHierarchy = Lens.lens (\ModuleInfo' {typeHierarchy} -> typeHierarchy) (\s@ModuleInfo' {} a -> s {typeHierarchy = a} :: ModuleInfo)

instance Data.FromXML ModuleInfo where
  parseXML x =
    ModuleInfo'
      Prelude.<$> (x Data..@? "LogicalIdHierarchy")
      Prelude.<*> (x Data..@? "TypeHierarchy")

instance Prelude.Hashable ModuleInfo where
  hashWithSalt _salt ModuleInfo' {..} =
    _salt `Prelude.hashWithSalt` logicalIdHierarchy
      `Prelude.hashWithSalt` typeHierarchy

instance Prelude.NFData ModuleInfo where
  rnf ModuleInfo' {..} =
    Prelude.rnf logicalIdHierarchy
      `Prelude.seq` Prelude.rnf typeHierarchy
