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
-- Module      : Amazonka.AppRunner.Types.SourceCodeVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.SourceCodeVersion where

import Amazonka.AppRunner.Types.SourceCodeVersionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies a version of code that App Runner refers to within a source
-- code repository.
--
-- /See:/ 'newSourceCodeVersion' smart constructor.
data SourceCodeVersion = SourceCodeVersion'
  { -- | The type of version identifier.
    --
    -- For a git-based repository, branches represent versions.
    type' :: SourceCodeVersionType,
    -- | A source code version.
    --
    -- For a git-based repository, a branch name maps to a specific version.
    -- App Runner uses the most recent commit to the branch.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceCodeVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'sourceCodeVersion_type' - The type of version identifier.
--
-- For a git-based repository, branches represent versions.
--
-- 'value', 'sourceCodeVersion_value' - A source code version.
--
-- For a git-based repository, a branch name maps to a specific version.
-- App Runner uses the most recent commit to the branch.
newSourceCodeVersion ::
  -- | 'type''
  SourceCodeVersionType ->
  -- | 'value'
  Prelude.Text ->
  SourceCodeVersion
newSourceCodeVersion pType_ pValue_ =
  SourceCodeVersion' {type' = pType_, value = pValue_}

-- | The type of version identifier.
--
-- For a git-based repository, branches represent versions.
sourceCodeVersion_type :: Lens.Lens' SourceCodeVersion SourceCodeVersionType
sourceCodeVersion_type = Lens.lens (\SourceCodeVersion' {type'} -> type') (\s@SourceCodeVersion' {} a -> s {type' = a} :: SourceCodeVersion)

-- | A source code version.
--
-- For a git-based repository, a branch name maps to a specific version.
-- App Runner uses the most recent commit to the branch.
sourceCodeVersion_value :: Lens.Lens' SourceCodeVersion Prelude.Text
sourceCodeVersion_value = Lens.lens (\SourceCodeVersion' {value} -> value) (\s@SourceCodeVersion' {} a -> s {value = a} :: SourceCodeVersion)

instance Data.FromJSON SourceCodeVersion where
  parseJSON =
    Data.withObject
      "SourceCodeVersion"
      ( \x ->
          SourceCodeVersion'
            Prelude.<$> (x Data..: "Type") Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable SourceCodeVersion where
  hashWithSalt _salt SourceCodeVersion' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData SourceCodeVersion where
  rnf SourceCodeVersion' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Data.ToJSON SourceCodeVersion where
  toJSON SourceCodeVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Value" Data..= value)
          ]
      )
