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
-- Module      : Amazonka.Glue.Types.CustomCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CustomCode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.GlueSchema
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that uses custom code you provide to perform the
-- data transformation. The output is a collection of DynamicFrames.
--
-- /See:/ 'newCustomCode' smart constructor.
data CustomCode = CustomCode'
  { -- | Specifies the data schema for the custom code transform.
    outputSchemas :: Prelude.Maybe [GlueSchema],
    -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | The custom code that is used to perform the data transformation.
    code :: Prelude.Text,
    -- | The name defined for the custom code node class.
    className :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomCode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputSchemas', 'customCode_outputSchemas' - Specifies the data schema for the custom code transform.
--
-- 'name', 'customCode_name' - The name of the transform node.
--
-- 'inputs', 'customCode_inputs' - The data inputs identified by their node names.
--
-- 'code', 'customCode_code' - The custom code that is used to perform the data transformation.
--
-- 'className', 'customCode_className' - The name defined for the custom code node class.
newCustomCode ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'code'
  Prelude.Text ->
  -- | 'className'
  Prelude.Text ->
  CustomCode
newCustomCode pName_ pInputs_ pCode_ pClassName_ =
  CustomCode'
    { outputSchemas = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      code = pCode_,
      className = pClassName_
    }

-- | Specifies the data schema for the custom code transform.
customCode_outputSchemas :: Lens.Lens' CustomCode (Prelude.Maybe [GlueSchema])
customCode_outputSchemas = Lens.lens (\CustomCode' {outputSchemas} -> outputSchemas) (\s@CustomCode' {} a -> s {outputSchemas = a} :: CustomCode) Prelude.. Lens.mapping Lens.coerced

-- | The name of the transform node.
customCode_name :: Lens.Lens' CustomCode Prelude.Text
customCode_name = Lens.lens (\CustomCode' {name} -> name) (\s@CustomCode' {} a -> s {name = a} :: CustomCode)

-- | The data inputs identified by their node names.
customCode_inputs :: Lens.Lens' CustomCode (Prelude.NonEmpty Prelude.Text)
customCode_inputs = Lens.lens (\CustomCode' {inputs} -> inputs) (\s@CustomCode' {} a -> s {inputs = a} :: CustomCode) Prelude.. Lens.coerced

-- | The custom code that is used to perform the data transformation.
customCode_code :: Lens.Lens' CustomCode Prelude.Text
customCode_code = Lens.lens (\CustomCode' {code} -> code) (\s@CustomCode' {} a -> s {code = a} :: CustomCode)

-- | The name defined for the custom code node class.
customCode_className :: Lens.Lens' CustomCode Prelude.Text
customCode_className = Lens.lens (\CustomCode' {className} -> className) (\s@CustomCode' {} a -> s {className = a} :: CustomCode)

instance Data.FromJSON CustomCode where
  parseJSON =
    Data.withObject
      "CustomCode"
      ( \x ->
          CustomCode'
            Prelude.<$> (x Data..:? "OutputSchemas" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "Code")
            Prelude.<*> (x Data..: "ClassName")
      )

instance Prelude.Hashable CustomCode where
  hashWithSalt _salt CustomCode' {..} =
    _salt
      `Prelude.hashWithSalt` outputSchemas
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` className

instance Prelude.NFData CustomCode where
  rnf CustomCode' {..} =
    Prelude.rnf outputSchemas
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf className

instance Data.ToJSON CustomCode where
  toJSON CustomCode' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OutputSchemas" Data..=) Prelude.<$> outputSchemas,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("Code" Data..= code),
            Prelude.Just ("ClassName" Data..= className)
          ]
      )
