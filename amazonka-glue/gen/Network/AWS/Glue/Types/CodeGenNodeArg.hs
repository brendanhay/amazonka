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
-- Module      : Network.AWS.Glue.Types.CodeGenNodeArg
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenNodeArg where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An argument or property of a node.
--
-- /See:/ 'newCodeGenNodeArg' smart constructor.
data CodeGenNodeArg = CodeGenNodeArg'
  { -- | True if the value is used as a parameter.
    param :: Core.Maybe Core.Bool,
    -- | The name of the argument or property.
    name :: Core.Text,
    -- | The value of the argument or property.
    value :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CodeGenNodeArg' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'param', 'codeGenNodeArg_param' - True if the value is used as a parameter.
--
-- 'name', 'codeGenNodeArg_name' - The name of the argument or property.
--
-- 'value', 'codeGenNodeArg_value' - The value of the argument or property.
newCodeGenNodeArg ::
  -- | 'name'
  Core.Text ->
  -- | 'value'
  Core.Text ->
  CodeGenNodeArg
newCodeGenNodeArg pName_ pValue_ =
  CodeGenNodeArg'
    { param = Core.Nothing,
      name = pName_,
      value = pValue_
    }

-- | True if the value is used as a parameter.
codeGenNodeArg_param :: Lens.Lens' CodeGenNodeArg (Core.Maybe Core.Bool)
codeGenNodeArg_param = Lens.lens (\CodeGenNodeArg' {param} -> param) (\s@CodeGenNodeArg' {} a -> s {param = a} :: CodeGenNodeArg)

-- | The name of the argument or property.
codeGenNodeArg_name :: Lens.Lens' CodeGenNodeArg Core.Text
codeGenNodeArg_name = Lens.lens (\CodeGenNodeArg' {name} -> name) (\s@CodeGenNodeArg' {} a -> s {name = a} :: CodeGenNodeArg)

-- | The value of the argument or property.
codeGenNodeArg_value :: Lens.Lens' CodeGenNodeArg Core.Text
codeGenNodeArg_value = Lens.lens (\CodeGenNodeArg' {value} -> value) (\s@CodeGenNodeArg' {} a -> s {value = a} :: CodeGenNodeArg)

instance Core.FromJSON CodeGenNodeArg where
  parseJSON =
    Core.withObject
      "CodeGenNodeArg"
      ( \x ->
          CodeGenNodeArg'
            Core.<$> (x Core..:? "Param")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "Value")
      )

instance Core.Hashable CodeGenNodeArg

instance Core.NFData CodeGenNodeArg

instance Core.ToJSON CodeGenNodeArg where
  toJSON CodeGenNodeArg' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Param" Core..=) Core.<$> param,
            Core.Just ("Name" Core..= name),
            Core.Just ("Value" Core..= value)
          ]
      )
