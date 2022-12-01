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
-- Module      : Amazonka.SageMaker.Types.CodeRepositorySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CodeRepositorySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.GitConfig

-- | Specifies summary information about a Git repository.
--
-- /See:/ 'newCodeRepositorySummary' smart constructor.
data CodeRepositorySummary = CodeRepositorySummary'
  { -- | Configuration details for the Git repository, including the URL where it
    -- is located and the ARN of the Amazon Web Services Secrets Manager secret
    -- that contains the credentials used to access the repository.
    gitConfig :: Prelude.Maybe GitConfig,
    -- | The name of the Git repository.
    codeRepositoryName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Git repository.
    codeRepositoryArn :: Prelude.Text,
    -- | The date and time that the Git repository was created.
    creationTime :: Core.POSIX,
    -- | The date and time that the Git repository was last modified.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodeRepositorySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gitConfig', 'codeRepositorySummary_gitConfig' - Configuration details for the Git repository, including the URL where it
-- is located and the ARN of the Amazon Web Services Secrets Manager secret
-- that contains the credentials used to access the repository.
--
-- 'codeRepositoryName', 'codeRepositorySummary_codeRepositoryName' - The name of the Git repository.
--
-- 'codeRepositoryArn', 'codeRepositorySummary_codeRepositoryArn' - The Amazon Resource Name (ARN) of the Git repository.
--
-- 'creationTime', 'codeRepositorySummary_creationTime' - The date and time that the Git repository was created.
--
-- 'lastModifiedTime', 'codeRepositorySummary_lastModifiedTime' - The date and time that the Git repository was last modified.
newCodeRepositorySummary ::
  -- | 'codeRepositoryName'
  Prelude.Text ->
  -- | 'codeRepositoryArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  CodeRepositorySummary
newCodeRepositorySummary
  pCodeRepositoryName_
  pCodeRepositoryArn_
  pCreationTime_
  pLastModifiedTime_ =
    CodeRepositorySummary'
      { gitConfig = Prelude.Nothing,
        codeRepositoryName = pCodeRepositoryName_,
        codeRepositoryArn = pCodeRepositoryArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | Configuration details for the Git repository, including the URL where it
-- is located and the ARN of the Amazon Web Services Secrets Manager secret
-- that contains the credentials used to access the repository.
codeRepositorySummary_gitConfig :: Lens.Lens' CodeRepositorySummary (Prelude.Maybe GitConfig)
codeRepositorySummary_gitConfig = Lens.lens (\CodeRepositorySummary' {gitConfig} -> gitConfig) (\s@CodeRepositorySummary' {} a -> s {gitConfig = a} :: CodeRepositorySummary)

-- | The name of the Git repository.
codeRepositorySummary_codeRepositoryName :: Lens.Lens' CodeRepositorySummary Prelude.Text
codeRepositorySummary_codeRepositoryName = Lens.lens (\CodeRepositorySummary' {codeRepositoryName} -> codeRepositoryName) (\s@CodeRepositorySummary' {} a -> s {codeRepositoryName = a} :: CodeRepositorySummary)

-- | The Amazon Resource Name (ARN) of the Git repository.
codeRepositorySummary_codeRepositoryArn :: Lens.Lens' CodeRepositorySummary Prelude.Text
codeRepositorySummary_codeRepositoryArn = Lens.lens (\CodeRepositorySummary' {codeRepositoryArn} -> codeRepositoryArn) (\s@CodeRepositorySummary' {} a -> s {codeRepositoryArn = a} :: CodeRepositorySummary)

-- | The date and time that the Git repository was created.
codeRepositorySummary_creationTime :: Lens.Lens' CodeRepositorySummary Prelude.UTCTime
codeRepositorySummary_creationTime = Lens.lens (\CodeRepositorySummary' {creationTime} -> creationTime) (\s@CodeRepositorySummary' {} a -> s {creationTime = a} :: CodeRepositorySummary) Prelude.. Core._Time

-- | The date and time that the Git repository was last modified.
codeRepositorySummary_lastModifiedTime :: Lens.Lens' CodeRepositorySummary Prelude.UTCTime
codeRepositorySummary_lastModifiedTime = Lens.lens (\CodeRepositorySummary' {lastModifiedTime} -> lastModifiedTime) (\s@CodeRepositorySummary' {} a -> s {lastModifiedTime = a} :: CodeRepositorySummary) Prelude.. Core._Time

instance Core.FromJSON CodeRepositorySummary where
  parseJSON =
    Core.withObject
      "CodeRepositorySummary"
      ( \x ->
          CodeRepositorySummary'
            Prelude.<$> (x Core..:? "GitConfig")
            Prelude.<*> (x Core..: "CodeRepositoryName")
            Prelude.<*> (x Core..: "CodeRepositoryArn")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "LastModifiedTime")
      )

instance Prelude.Hashable CodeRepositorySummary where
  hashWithSalt _salt CodeRepositorySummary' {..} =
    _salt `Prelude.hashWithSalt` gitConfig
      `Prelude.hashWithSalt` codeRepositoryName
      `Prelude.hashWithSalt` codeRepositoryArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData CodeRepositorySummary where
  rnf CodeRepositorySummary' {..} =
    Prelude.rnf gitConfig
      `Prelude.seq` Prelude.rnf codeRepositoryName
      `Prelude.seq` Prelude.rnf codeRepositoryArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
