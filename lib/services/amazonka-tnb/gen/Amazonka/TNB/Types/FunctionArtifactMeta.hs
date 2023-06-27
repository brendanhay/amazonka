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
-- Module      : Amazonka.TNB.Types.FunctionArtifactMeta
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TNB.Types.FunctionArtifactMeta where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TNB.Types.ToscaOverride

-- | Metadata for function package artifacts.
--
-- Artifacts are the contents of the package descriptor file and the state
-- of the package.
--
-- /See:/ 'newFunctionArtifactMeta' smart constructor.
data FunctionArtifactMeta = FunctionArtifactMeta'
  { -- | Lists of function package overrides.
    overrides :: Prelude.Maybe [ToscaOverride]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionArtifactMeta' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrides', 'functionArtifactMeta_overrides' - Lists of function package overrides.
newFunctionArtifactMeta ::
  FunctionArtifactMeta
newFunctionArtifactMeta =
  FunctionArtifactMeta' {overrides = Prelude.Nothing}

-- | Lists of function package overrides.
functionArtifactMeta_overrides :: Lens.Lens' FunctionArtifactMeta (Prelude.Maybe [ToscaOverride])
functionArtifactMeta_overrides = Lens.lens (\FunctionArtifactMeta' {overrides} -> overrides) (\s@FunctionArtifactMeta' {} a -> s {overrides = a} :: FunctionArtifactMeta) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FunctionArtifactMeta where
  parseJSON =
    Data.withObject
      "FunctionArtifactMeta"
      ( \x ->
          FunctionArtifactMeta'
            Prelude.<$> (x Data..:? "overrides" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FunctionArtifactMeta where
  hashWithSalt _salt FunctionArtifactMeta' {..} =
    _salt `Prelude.hashWithSalt` overrides

instance Prelude.NFData FunctionArtifactMeta where
  rnf FunctionArtifactMeta' {..} = Prelude.rnf overrides
