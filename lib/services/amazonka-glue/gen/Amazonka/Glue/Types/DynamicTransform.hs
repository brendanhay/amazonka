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
-- Module      : Amazonka.Glue.Types.DynamicTransform
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DynamicTransform where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.TransformConfigParameter
import qualified Amazonka.Prelude as Prelude

-- | Specifies the set of parameters needed to perform the dynamic transform.
--
-- /See:/ 'newDynamicTransform' smart constructor.
data DynamicTransform = DynamicTransform'
  { -- | Specifies the parameters of the dynamic transform.
    parameters :: Prelude.Maybe [TransformConfigParameter],
    -- | This field is not used and will be deprecated in future release.
    version :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the dynamic transform.
    name :: Prelude.Text,
    -- | Specifies the name of the dynamic transform as it appears in the Glue
    -- Studio visual editor.
    transformName :: Prelude.Text,
    -- | Specifies the inputs for the dynamic transform that are required.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | Specifies the name of the function of the dynamic transform.
    functionName :: Prelude.Text,
    -- | Specifies the path of the dynamic transform source and config files.
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynamicTransform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameters', 'dynamicTransform_parameters' - Specifies the parameters of the dynamic transform.
--
-- 'version', 'dynamicTransform_version' - This field is not used and will be deprecated in future release.
--
-- 'name', 'dynamicTransform_name' - Specifies the name of the dynamic transform.
--
-- 'transformName', 'dynamicTransform_transformName' - Specifies the name of the dynamic transform as it appears in the Glue
-- Studio visual editor.
--
-- 'inputs', 'dynamicTransform_inputs' - Specifies the inputs for the dynamic transform that are required.
--
-- 'functionName', 'dynamicTransform_functionName' - Specifies the name of the function of the dynamic transform.
--
-- 'path', 'dynamicTransform_path' - Specifies the path of the dynamic transform source and config files.
newDynamicTransform ::
  -- | 'name'
  Prelude.Text ->
  -- | 'transformName'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'functionName'
  Prelude.Text ->
  -- | 'path'
  Prelude.Text ->
  DynamicTransform
newDynamicTransform
  pName_
  pTransformName_
  pInputs_
  pFunctionName_
  pPath_ =
    DynamicTransform'
      { parameters = Prelude.Nothing,
        version = Prelude.Nothing,
        name = pName_,
        transformName = pTransformName_,
        inputs = Lens.coerced Lens.# pInputs_,
        functionName = pFunctionName_,
        path = pPath_
      }

-- | Specifies the parameters of the dynamic transform.
dynamicTransform_parameters :: Lens.Lens' DynamicTransform (Prelude.Maybe [TransformConfigParameter])
dynamicTransform_parameters = Lens.lens (\DynamicTransform' {parameters} -> parameters) (\s@DynamicTransform' {} a -> s {parameters = a} :: DynamicTransform) Prelude.. Lens.mapping Lens.coerced

-- | This field is not used and will be deprecated in future release.
dynamicTransform_version :: Lens.Lens' DynamicTransform (Prelude.Maybe Prelude.Text)
dynamicTransform_version = Lens.lens (\DynamicTransform' {version} -> version) (\s@DynamicTransform' {} a -> s {version = a} :: DynamicTransform)

-- | Specifies the name of the dynamic transform.
dynamicTransform_name :: Lens.Lens' DynamicTransform Prelude.Text
dynamicTransform_name = Lens.lens (\DynamicTransform' {name} -> name) (\s@DynamicTransform' {} a -> s {name = a} :: DynamicTransform)

-- | Specifies the name of the dynamic transform as it appears in the Glue
-- Studio visual editor.
dynamicTransform_transformName :: Lens.Lens' DynamicTransform Prelude.Text
dynamicTransform_transformName = Lens.lens (\DynamicTransform' {transformName} -> transformName) (\s@DynamicTransform' {} a -> s {transformName = a} :: DynamicTransform)

-- | Specifies the inputs for the dynamic transform that are required.
dynamicTransform_inputs :: Lens.Lens' DynamicTransform (Prelude.NonEmpty Prelude.Text)
dynamicTransform_inputs = Lens.lens (\DynamicTransform' {inputs} -> inputs) (\s@DynamicTransform' {} a -> s {inputs = a} :: DynamicTransform) Prelude.. Lens.coerced

-- | Specifies the name of the function of the dynamic transform.
dynamicTransform_functionName :: Lens.Lens' DynamicTransform Prelude.Text
dynamicTransform_functionName = Lens.lens (\DynamicTransform' {functionName} -> functionName) (\s@DynamicTransform' {} a -> s {functionName = a} :: DynamicTransform)

-- | Specifies the path of the dynamic transform source and config files.
dynamicTransform_path :: Lens.Lens' DynamicTransform Prelude.Text
dynamicTransform_path = Lens.lens (\DynamicTransform' {path} -> path) (\s@DynamicTransform' {} a -> s {path = a} :: DynamicTransform)

instance Data.FromJSON DynamicTransform where
  parseJSON =
    Data.withObject
      "DynamicTransform"
      ( \x ->
          DynamicTransform'
            Prelude.<$> (x Data..:? "Parameters" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "TransformName")
            Prelude.<*> (x Data..: "Inputs")
            Prelude.<*> (x Data..: "FunctionName")
            Prelude.<*> (x Data..: "Path")
      )

instance Prelude.Hashable DynamicTransform where
  hashWithSalt _salt DynamicTransform' {..} =
    _salt
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` transformName
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` path

instance Prelude.NFData DynamicTransform where
  rnf DynamicTransform' {..} =
    Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf transformName
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf path

instance Data.ToJSON DynamicTransform where
  toJSON DynamicTransform' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Parameters" Data..=) Prelude.<$> parameters,
            ("Version" Data..=) Prelude.<$> version,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("TransformName" Data..= transformName),
            Prelude.Just ("Inputs" Data..= inputs),
            Prelude.Just ("FunctionName" Data..= functionName),
            Prelude.Just ("Path" Data..= path)
          ]
      )
