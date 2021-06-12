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
-- Module      : Network.AWS.CloudFormation.Types.Export
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.Export where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The @Export@ structure describes the exported output values for a stack.
--
-- /See:/ 'newExport' smart constructor.
data Export = Export'
  { -- | The stack that contains the exported output name and value.
    exportingStackId :: Core.Maybe Core.Text,
    -- | The name of exported output value. Use this name and the
    -- @Fn::ImportValue@ function to import the associated value into other
    -- stacks. The name is defined in the @Export@ field in the associated
    -- stack\'s @Outputs@ section.
    name :: Core.Maybe Core.Text,
    -- | The value of the exported output, such as a resource physical ID. This
    -- value is defined in the @Export@ field in the associated stack\'s
    -- @Outputs@ section.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Export' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportingStackId', 'export_exportingStackId' - The stack that contains the exported output name and value.
--
-- 'name', 'export_name' - The name of exported output value. Use this name and the
-- @Fn::ImportValue@ function to import the associated value into other
-- stacks. The name is defined in the @Export@ field in the associated
-- stack\'s @Outputs@ section.
--
-- 'value', 'export_value' - The value of the exported output, such as a resource physical ID. This
-- value is defined in the @Export@ field in the associated stack\'s
-- @Outputs@ section.
newExport ::
  Export
newExport =
  Export'
    { exportingStackId = Core.Nothing,
      name = Core.Nothing,
      value = Core.Nothing
    }

-- | The stack that contains the exported output name and value.
export_exportingStackId :: Lens.Lens' Export (Core.Maybe Core.Text)
export_exportingStackId = Lens.lens (\Export' {exportingStackId} -> exportingStackId) (\s@Export' {} a -> s {exportingStackId = a} :: Export)

-- | The name of exported output value. Use this name and the
-- @Fn::ImportValue@ function to import the associated value into other
-- stacks. The name is defined in the @Export@ field in the associated
-- stack\'s @Outputs@ section.
export_name :: Lens.Lens' Export (Core.Maybe Core.Text)
export_name = Lens.lens (\Export' {name} -> name) (\s@Export' {} a -> s {name = a} :: Export)

-- | The value of the exported output, such as a resource physical ID. This
-- value is defined in the @Export@ field in the associated stack\'s
-- @Outputs@ section.
export_value :: Lens.Lens' Export (Core.Maybe Core.Text)
export_value = Lens.lens (\Export' {value} -> value) (\s@Export' {} a -> s {value = a} :: Export)

instance Core.FromXML Export where
  parseXML x =
    Export'
      Core.<$> (x Core..@? "ExportingStackId")
      Core.<*> (x Core..@? "Name")
      Core.<*> (x Core..@? "Value")

instance Core.Hashable Export

instance Core.NFData Export
