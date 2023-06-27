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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ReactStartCodegenJobData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ReactStartCodegenJobData where

import Amazonka.AmplifyUiBuilder.Types.JSModule
import Amazonka.AmplifyUiBuilder.Types.JSScript
import Amazonka.AmplifyUiBuilder.Types.JSTarget
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the code generation job configuration for a React project.
--
-- /See:/ 'newReactStartCodegenJobData' smart constructor.
data ReactStartCodegenJobData = ReactStartCodegenJobData'
  { -- | Specifies whether the code generation job should render inline source
    -- maps.
    inlineSourceMap :: Prelude.Maybe Prelude.Bool,
    -- | The JavaScript module type.
    module' :: Prelude.Maybe JSModule,
    -- | Specifies whether the code generation job should render type declaration
    -- files.
    renderTypeDeclarations :: Prelude.Maybe Prelude.Bool,
    -- | The file type to use for a JavaScript project.
    script :: Prelude.Maybe JSScript,
    -- | The ECMAScript specification to use.
    target :: Prelude.Maybe JSTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReactStartCodegenJobData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inlineSourceMap', 'reactStartCodegenJobData_inlineSourceMap' - Specifies whether the code generation job should render inline source
-- maps.
--
-- 'module'', 'reactStartCodegenJobData_module' - The JavaScript module type.
--
-- 'renderTypeDeclarations', 'reactStartCodegenJobData_renderTypeDeclarations' - Specifies whether the code generation job should render type declaration
-- files.
--
-- 'script', 'reactStartCodegenJobData_script' - The file type to use for a JavaScript project.
--
-- 'target', 'reactStartCodegenJobData_target' - The ECMAScript specification to use.
newReactStartCodegenJobData ::
  ReactStartCodegenJobData
newReactStartCodegenJobData =
  ReactStartCodegenJobData'
    { inlineSourceMap =
        Prelude.Nothing,
      module' = Prelude.Nothing,
      renderTypeDeclarations = Prelude.Nothing,
      script = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | Specifies whether the code generation job should render inline source
-- maps.
reactStartCodegenJobData_inlineSourceMap :: Lens.Lens' ReactStartCodegenJobData (Prelude.Maybe Prelude.Bool)
reactStartCodegenJobData_inlineSourceMap = Lens.lens (\ReactStartCodegenJobData' {inlineSourceMap} -> inlineSourceMap) (\s@ReactStartCodegenJobData' {} a -> s {inlineSourceMap = a} :: ReactStartCodegenJobData)

-- | The JavaScript module type.
reactStartCodegenJobData_module :: Lens.Lens' ReactStartCodegenJobData (Prelude.Maybe JSModule)
reactStartCodegenJobData_module = Lens.lens (\ReactStartCodegenJobData' {module'} -> module') (\s@ReactStartCodegenJobData' {} a -> s {module' = a} :: ReactStartCodegenJobData)

-- | Specifies whether the code generation job should render type declaration
-- files.
reactStartCodegenJobData_renderTypeDeclarations :: Lens.Lens' ReactStartCodegenJobData (Prelude.Maybe Prelude.Bool)
reactStartCodegenJobData_renderTypeDeclarations = Lens.lens (\ReactStartCodegenJobData' {renderTypeDeclarations} -> renderTypeDeclarations) (\s@ReactStartCodegenJobData' {} a -> s {renderTypeDeclarations = a} :: ReactStartCodegenJobData)

-- | The file type to use for a JavaScript project.
reactStartCodegenJobData_script :: Lens.Lens' ReactStartCodegenJobData (Prelude.Maybe JSScript)
reactStartCodegenJobData_script = Lens.lens (\ReactStartCodegenJobData' {script} -> script) (\s@ReactStartCodegenJobData' {} a -> s {script = a} :: ReactStartCodegenJobData)

-- | The ECMAScript specification to use.
reactStartCodegenJobData_target :: Lens.Lens' ReactStartCodegenJobData (Prelude.Maybe JSTarget)
reactStartCodegenJobData_target = Lens.lens (\ReactStartCodegenJobData' {target} -> target) (\s@ReactStartCodegenJobData' {} a -> s {target = a} :: ReactStartCodegenJobData)

instance Data.FromJSON ReactStartCodegenJobData where
  parseJSON =
    Data.withObject
      "ReactStartCodegenJobData"
      ( \x ->
          ReactStartCodegenJobData'
            Prelude.<$> (x Data..:? "inlineSourceMap")
            Prelude.<*> (x Data..:? "module")
            Prelude.<*> (x Data..:? "renderTypeDeclarations")
            Prelude.<*> (x Data..:? "script")
            Prelude.<*> (x Data..:? "target")
      )

instance Prelude.Hashable ReactStartCodegenJobData where
  hashWithSalt _salt ReactStartCodegenJobData' {..} =
    _salt
      `Prelude.hashWithSalt` inlineSourceMap
      `Prelude.hashWithSalt` module'
      `Prelude.hashWithSalt` renderTypeDeclarations
      `Prelude.hashWithSalt` script
      `Prelude.hashWithSalt` target

instance Prelude.NFData ReactStartCodegenJobData where
  rnf ReactStartCodegenJobData' {..} =
    Prelude.rnf inlineSourceMap
      `Prelude.seq` Prelude.rnf module'
      `Prelude.seq` Prelude.rnf renderTypeDeclarations
      `Prelude.seq` Prelude.rnf script
      `Prelude.seq` Prelude.rnf target

instance Data.ToJSON ReactStartCodegenJobData where
  toJSON ReactStartCodegenJobData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inlineSourceMap" Data..=)
              Prelude.<$> inlineSourceMap,
            ("module" Data..=) Prelude.<$> module',
            ("renderTypeDeclarations" Data..=)
              Prelude.<$> renderTypeDeclarations,
            ("script" Data..=) Prelude.<$> script,
            ("target" Data..=) Prelude.<$> target
          ]
      )
