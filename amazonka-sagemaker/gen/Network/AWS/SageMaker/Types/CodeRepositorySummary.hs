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
-- Module      : Network.AWS.SageMaker.Types.CodeRepositorySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CodeRepositorySummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.GitConfig

-- | Specifies summary information about a Git repository.
--
-- /See:/ 'newCodeRepositorySummary' smart constructor.
data CodeRepositorySummary = CodeRepositorySummary'
  { -- | Configuration details for the Git repository, including the URL where it
    -- is located and the ARN of the AWS Secrets Manager secret that contains
    -- the credentials used to access the repository.
    gitConfig :: Prelude.Maybe GitConfig,
    -- | The name of the Git repository.
    codeRepositoryName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Git repository.
    codeRepositoryArn :: Prelude.Text,
    -- | The date and time that the Git repository was created.
    creationTime :: Prelude.POSIX,
    -- | The date and time that the Git repository was last modified.
    lastModifiedTime :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CodeRepositorySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gitConfig', 'codeRepositorySummary_gitConfig' - Configuration details for the Git repository, including the URL where it
-- is located and the ARN of the AWS Secrets Manager secret that contains
-- the credentials used to access the repository.
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
        creationTime = Prelude._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Prelude._Time Lens.# pLastModifiedTime_
      }

-- | Configuration details for the Git repository, including the URL where it
-- is located and the ARN of the AWS Secrets Manager secret that contains
-- the credentials used to access the repository.
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
codeRepositorySummary_creationTime = Lens.lens (\CodeRepositorySummary' {creationTime} -> creationTime) (\s@CodeRepositorySummary' {} a -> s {creationTime = a} :: CodeRepositorySummary) Prelude.. Prelude._Time

-- | The date and time that the Git repository was last modified.
codeRepositorySummary_lastModifiedTime :: Lens.Lens' CodeRepositorySummary Prelude.UTCTime
codeRepositorySummary_lastModifiedTime = Lens.lens (\CodeRepositorySummary' {lastModifiedTime} -> lastModifiedTime) (\s@CodeRepositorySummary' {} a -> s {lastModifiedTime = a} :: CodeRepositorySummary) Prelude.. Prelude._Time

instance Prelude.FromJSON CodeRepositorySummary where
  parseJSON =
    Prelude.withObject
      "CodeRepositorySummary"
      ( \x ->
          CodeRepositorySummary'
            Prelude.<$> (x Prelude..:? "GitConfig")
            Prelude.<*> (x Prelude..: "CodeRepositoryName")
            Prelude.<*> (x Prelude..: "CodeRepositoryArn")
            Prelude.<*> (x Prelude..: "CreationTime")
            Prelude.<*> (x Prelude..: "LastModifiedTime")
      )

instance Prelude.Hashable CodeRepositorySummary

instance Prelude.NFData CodeRepositorySummary
