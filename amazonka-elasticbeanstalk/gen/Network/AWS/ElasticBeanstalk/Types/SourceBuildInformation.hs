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
-- Module      : Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation where

import Network.AWS.ElasticBeanstalk.Types.SourceRepository
import Network.AWS.ElasticBeanstalk.Types.SourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Location of the source code for an application version.
--
-- /See:/ 'newSourceBuildInformation' smart constructor.
data SourceBuildInformation = SourceBuildInformation'
  { -- | The type of repository.
    --
    -- -   @Git@
    --
    -- -   @Zip@
    sourceType :: SourceType,
    -- | Location where the repository is stored.
    --
    -- -   @CodeCommit@
    --
    -- -   @S3@
    sourceRepository :: SourceRepository,
    -- | The location of the source code, as a formatted string, depending on the
    -- value of @SourceRepository@
    --
    -- -   For @CodeCommit@, the format is the repository name and commit ID,
    --     separated by a forward slash. For example,
    --     @my-git-repo\/265cfa0cf6af46153527f55d6503ec030551f57a@.
    --
    -- -   For @S3@, the format is the S3 bucket name and object key, separated
    --     by a forward slash. For example,
    --     @my-s3-bucket\/Folders\/my-source-file@.
    sourceLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SourceBuildInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceType', 'sourceBuildInformation_sourceType' - The type of repository.
--
-- -   @Git@
--
-- -   @Zip@
--
-- 'sourceRepository', 'sourceBuildInformation_sourceRepository' - Location where the repository is stored.
--
-- -   @CodeCommit@
--
-- -   @S3@
--
-- 'sourceLocation', 'sourceBuildInformation_sourceLocation' - The location of the source code, as a formatted string, depending on the
-- value of @SourceRepository@
--
-- -   For @CodeCommit@, the format is the repository name and commit ID,
--     separated by a forward slash. For example,
--     @my-git-repo\/265cfa0cf6af46153527f55d6503ec030551f57a@.
--
-- -   For @S3@, the format is the S3 bucket name and object key, separated
--     by a forward slash. For example,
--     @my-s3-bucket\/Folders\/my-source-file@.
newSourceBuildInformation ::
  -- | 'sourceType'
  SourceType ->
  -- | 'sourceRepository'
  SourceRepository ->
  -- | 'sourceLocation'
  Prelude.Text ->
  SourceBuildInformation
newSourceBuildInformation
  pSourceType_
  pSourceRepository_
  pSourceLocation_ =
    SourceBuildInformation'
      { sourceType = pSourceType_,
        sourceRepository = pSourceRepository_,
        sourceLocation = pSourceLocation_
      }

-- | The type of repository.
--
-- -   @Git@
--
-- -   @Zip@
sourceBuildInformation_sourceType :: Lens.Lens' SourceBuildInformation SourceType
sourceBuildInformation_sourceType = Lens.lens (\SourceBuildInformation' {sourceType} -> sourceType) (\s@SourceBuildInformation' {} a -> s {sourceType = a} :: SourceBuildInformation)

-- | Location where the repository is stored.
--
-- -   @CodeCommit@
--
-- -   @S3@
sourceBuildInformation_sourceRepository :: Lens.Lens' SourceBuildInformation SourceRepository
sourceBuildInformation_sourceRepository = Lens.lens (\SourceBuildInformation' {sourceRepository} -> sourceRepository) (\s@SourceBuildInformation' {} a -> s {sourceRepository = a} :: SourceBuildInformation)

-- | The location of the source code, as a formatted string, depending on the
-- value of @SourceRepository@
--
-- -   For @CodeCommit@, the format is the repository name and commit ID,
--     separated by a forward slash. For example,
--     @my-git-repo\/265cfa0cf6af46153527f55d6503ec030551f57a@.
--
-- -   For @S3@, the format is the S3 bucket name and object key, separated
--     by a forward slash. For example,
--     @my-s3-bucket\/Folders\/my-source-file@.
sourceBuildInformation_sourceLocation :: Lens.Lens' SourceBuildInformation Prelude.Text
sourceBuildInformation_sourceLocation = Lens.lens (\SourceBuildInformation' {sourceLocation} -> sourceLocation) (\s@SourceBuildInformation' {} a -> s {sourceLocation = a} :: SourceBuildInformation)

instance Prelude.FromXML SourceBuildInformation where
  parseXML x =
    SourceBuildInformation'
      Prelude.<$> (x Prelude..@ "SourceType")
      Prelude.<*> (x Prelude..@ "SourceRepository")
      Prelude.<*> (x Prelude..@ "SourceLocation")

instance Prelude.Hashable SourceBuildInformation

instance Prelude.NFData SourceBuildInformation

instance Prelude.ToQuery SourceBuildInformation where
  toQuery SourceBuildInformation' {..} =
    Prelude.mconcat
      [ "SourceType" Prelude.=: sourceType,
        "SourceRepository" Prelude.=: sourceRepository,
        "SourceLocation" Prelude.=: sourceLocation
      ]
