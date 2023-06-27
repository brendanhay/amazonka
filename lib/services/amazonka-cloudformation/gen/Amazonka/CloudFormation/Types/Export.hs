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
-- Module      : Amazonka.CloudFormation.Types.Export
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.Export where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The @Export@ structure describes the exported output values for a stack.
--
-- /See:/ 'newExport' smart constructor.
data Export = Export'
  { -- | The stack that contains the exported output name and value.
    exportingStackId :: Prelude.Maybe Prelude.Text,
    -- | The name of exported output value. Use this name and the
    -- @Fn::ImportValue@ function to import the associated value into other
    -- stacks. The name is defined in the @Export@ field in the associated
    -- stack\'s @Outputs@ section.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value of the exported output, such as a resource physical ID. This
    -- value is defined in the @Export@ field in the associated stack\'s
    -- @Outputs@ section.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { exportingStackId = Prelude.Nothing,
      name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The stack that contains the exported output name and value.
export_exportingStackId :: Lens.Lens' Export (Prelude.Maybe Prelude.Text)
export_exportingStackId = Lens.lens (\Export' {exportingStackId} -> exportingStackId) (\s@Export' {} a -> s {exportingStackId = a} :: Export)

-- | The name of exported output value. Use this name and the
-- @Fn::ImportValue@ function to import the associated value into other
-- stacks. The name is defined in the @Export@ field in the associated
-- stack\'s @Outputs@ section.
export_name :: Lens.Lens' Export (Prelude.Maybe Prelude.Text)
export_name = Lens.lens (\Export' {name} -> name) (\s@Export' {} a -> s {name = a} :: Export)

-- | The value of the exported output, such as a resource physical ID. This
-- value is defined in the @Export@ field in the associated stack\'s
-- @Outputs@ section.
export_value :: Lens.Lens' Export (Prelude.Maybe Prelude.Text)
export_value = Lens.lens (\Export' {value} -> value) (\s@Export' {} a -> s {value = a} :: Export)

instance Data.FromXML Export where
  parseXML x =
    Export'
      Prelude.<$> (x Data..@? "ExportingStackId")
      Prelude.<*> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "Value")

instance Prelude.Hashable Export where
  hashWithSalt _salt Export' {..} =
    _salt
      `Prelude.hashWithSalt` exportingStackId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData Export where
  rnf Export' {..} =
    Prelude.rnf exportingStackId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf value
