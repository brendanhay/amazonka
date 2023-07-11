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
-- Module      : Amazonka.SageMaker.Types.CodeRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CodeRepository where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A Git repository that SageMaker automatically displays to users for
-- cloning in the JupyterServer application.
--
-- /See:/ 'newCodeRepository' smart constructor.
data CodeRepository = CodeRepository'
  { -- | The URL of the Git repository.
    repositoryUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryUrl', 'codeRepository_repositoryUrl' - The URL of the Git repository.
newCodeRepository ::
  -- | 'repositoryUrl'
  Prelude.Text ->
  CodeRepository
newCodeRepository pRepositoryUrl_ =
  CodeRepository' {repositoryUrl = pRepositoryUrl_}

-- | The URL of the Git repository.
codeRepository_repositoryUrl :: Lens.Lens' CodeRepository Prelude.Text
codeRepository_repositoryUrl = Lens.lens (\CodeRepository' {repositoryUrl} -> repositoryUrl) (\s@CodeRepository' {} a -> s {repositoryUrl = a} :: CodeRepository)

instance Data.FromJSON CodeRepository where
  parseJSON =
    Data.withObject
      "CodeRepository"
      ( \x ->
          CodeRepository'
            Prelude.<$> (x Data..: "RepositoryUrl")
      )

instance Prelude.Hashable CodeRepository where
  hashWithSalt _salt CodeRepository' {..} =
    _salt `Prelude.hashWithSalt` repositoryUrl

instance Prelude.NFData CodeRepository where
  rnf CodeRepository' {..} = Prelude.rnf repositoryUrl

instance Data.ToJSON CodeRepository where
  toJSON CodeRepository' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RepositoryUrl" Data..= repositoryUrl)
          ]
      )
