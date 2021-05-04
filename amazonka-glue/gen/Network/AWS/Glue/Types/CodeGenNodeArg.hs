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
-- Module      : Network.AWS.Glue.Types.CodeGenNodeArg
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenNodeArg where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An argument or property of a node.
--
-- /See:/ 'newCodeGenNodeArg' smart constructor.
data CodeGenNodeArg = CodeGenNodeArg'
  { -- | True if the value is used as a parameter.
    param :: Prelude.Maybe Prelude.Bool,
    -- | The name of the argument or property.
    name :: Prelude.Text,
    -- | The value of the argument or property.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  CodeGenNodeArg
newCodeGenNodeArg pName_ pValue_ =
  CodeGenNodeArg'
    { param = Prelude.Nothing,
      name = pName_,
      value = pValue_
    }

-- | True if the value is used as a parameter.
codeGenNodeArg_param :: Lens.Lens' CodeGenNodeArg (Prelude.Maybe Prelude.Bool)
codeGenNodeArg_param = Lens.lens (\CodeGenNodeArg' {param} -> param) (\s@CodeGenNodeArg' {} a -> s {param = a} :: CodeGenNodeArg)

-- | The name of the argument or property.
codeGenNodeArg_name :: Lens.Lens' CodeGenNodeArg Prelude.Text
codeGenNodeArg_name = Lens.lens (\CodeGenNodeArg' {name} -> name) (\s@CodeGenNodeArg' {} a -> s {name = a} :: CodeGenNodeArg)

-- | The value of the argument or property.
codeGenNodeArg_value :: Lens.Lens' CodeGenNodeArg Prelude.Text
codeGenNodeArg_value = Lens.lens (\CodeGenNodeArg' {value} -> value) (\s@CodeGenNodeArg' {} a -> s {value = a} :: CodeGenNodeArg)

instance Prelude.FromJSON CodeGenNodeArg where
  parseJSON =
    Prelude.withObject
      "CodeGenNodeArg"
      ( \x ->
          CodeGenNodeArg'
            Prelude.<$> (x Prelude..:? "Param")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "Value")
      )

instance Prelude.Hashable CodeGenNodeArg

instance Prelude.NFData CodeGenNodeArg

instance Prelude.ToJSON CodeGenNodeArg where
  toJSON CodeGenNodeArg' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Param" Prelude..=) Prelude.<$> param,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Value" Prelude..= value)
          ]
      )
