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
-- Module      : Amazonka.MGN.Types.PostLaunchActions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.PostLaunchActions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.PostLaunchActionsDeploymentType
import Amazonka.MGN.Types.SsmDocument
import qualified Amazonka.Prelude as Prelude

-- | Post Launch Actions to executed on the Test or Cutover instance.
--
-- /See:/ 'newPostLaunchActions' smart constructor.
data PostLaunchActions = PostLaunchActions'
  { -- | AWS Systems Manager Command\'s CloudWatch log group name.
    cloudWatchLogGroupName :: Prelude.Maybe Prelude.Text,
    -- | Deployment type in which AWS Systems Manager Documents will be executed.
    deployment :: Prelude.Maybe PostLaunchActionsDeploymentType,
    -- | AWS Systems Manager Command\'s logs S3 log bucket.
    s3LogBucket :: Prelude.Maybe Prelude.Text,
    -- | AWS Systems Manager Command\'s logs S3 output key prefix.
    s3OutputKeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | AWS Systems Manager Documents.
    ssmDocuments :: Prelude.Maybe [SsmDocument]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostLaunchActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogGroupName', 'postLaunchActions_cloudWatchLogGroupName' - AWS Systems Manager Command\'s CloudWatch log group name.
--
-- 'deployment', 'postLaunchActions_deployment' - Deployment type in which AWS Systems Manager Documents will be executed.
--
-- 's3LogBucket', 'postLaunchActions_s3LogBucket' - AWS Systems Manager Command\'s logs S3 log bucket.
--
-- 's3OutputKeyPrefix', 'postLaunchActions_s3OutputKeyPrefix' - AWS Systems Manager Command\'s logs S3 output key prefix.
--
-- 'ssmDocuments', 'postLaunchActions_ssmDocuments' - AWS Systems Manager Documents.
newPostLaunchActions ::
  PostLaunchActions
newPostLaunchActions =
  PostLaunchActions'
    { cloudWatchLogGroupName =
        Prelude.Nothing,
      deployment = Prelude.Nothing,
      s3LogBucket = Prelude.Nothing,
      s3OutputKeyPrefix = Prelude.Nothing,
      ssmDocuments = Prelude.Nothing
    }

-- | AWS Systems Manager Command\'s CloudWatch log group name.
postLaunchActions_cloudWatchLogGroupName :: Lens.Lens' PostLaunchActions (Prelude.Maybe Prelude.Text)
postLaunchActions_cloudWatchLogGroupName = Lens.lens (\PostLaunchActions' {cloudWatchLogGroupName} -> cloudWatchLogGroupName) (\s@PostLaunchActions' {} a -> s {cloudWatchLogGroupName = a} :: PostLaunchActions)

-- | Deployment type in which AWS Systems Manager Documents will be executed.
postLaunchActions_deployment :: Lens.Lens' PostLaunchActions (Prelude.Maybe PostLaunchActionsDeploymentType)
postLaunchActions_deployment = Lens.lens (\PostLaunchActions' {deployment} -> deployment) (\s@PostLaunchActions' {} a -> s {deployment = a} :: PostLaunchActions)

-- | AWS Systems Manager Command\'s logs S3 log bucket.
postLaunchActions_s3LogBucket :: Lens.Lens' PostLaunchActions (Prelude.Maybe Prelude.Text)
postLaunchActions_s3LogBucket = Lens.lens (\PostLaunchActions' {s3LogBucket} -> s3LogBucket) (\s@PostLaunchActions' {} a -> s {s3LogBucket = a} :: PostLaunchActions)

-- | AWS Systems Manager Command\'s logs S3 output key prefix.
postLaunchActions_s3OutputKeyPrefix :: Lens.Lens' PostLaunchActions (Prelude.Maybe Prelude.Text)
postLaunchActions_s3OutputKeyPrefix = Lens.lens (\PostLaunchActions' {s3OutputKeyPrefix} -> s3OutputKeyPrefix) (\s@PostLaunchActions' {} a -> s {s3OutputKeyPrefix = a} :: PostLaunchActions)

-- | AWS Systems Manager Documents.
postLaunchActions_ssmDocuments :: Lens.Lens' PostLaunchActions (Prelude.Maybe [SsmDocument])
postLaunchActions_ssmDocuments = Lens.lens (\PostLaunchActions' {ssmDocuments} -> ssmDocuments) (\s@PostLaunchActions' {} a -> s {ssmDocuments = a} :: PostLaunchActions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PostLaunchActions where
  parseJSON =
    Data.withObject
      "PostLaunchActions"
      ( \x ->
          PostLaunchActions'
            Prelude.<$> (x Data..:? "cloudWatchLogGroupName")
            Prelude.<*> (x Data..:? "deployment")
            Prelude.<*> (x Data..:? "s3LogBucket")
            Prelude.<*> (x Data..:? "s3OutputKeyPrefix")
            Prelude.<*> (x Data..:? "ssmDocuments" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PostLaunchActions where
  hashWithSalt _salt PostLaunchActions' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogGroupName
      `Prelude.hashWithSalt` deployment
      `Prelude.hashWithSalt` s3LogBucket
      `Prelude.hashWithSalt` s3OutputKeyPrefix
      `Prelude.hashWithSalt` ssmDocuments

instance Prelude.NFData PostLaunchActions where
  rnf PostLaunchActions' {..} =
    Prelude.rnf cloudWatchLogGroupName
      `Prelude.seq` Prelude.rnf deployment
      `Prelude.seq` Prelude.rnf s3LogBucket
      `Prelude.seq` Prelude.rnf s3OutputKeyPrefix
      `Prelude.seq` Prelude.rnf ssmDocuments

instance Data.ToJSON PostLaunchActions where
  toJSON PostLaunchActions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cloudWatchLogGroupName" Data..=)
              Prelude.<$> cloudWatchLogGroupName,
            ("deployment" Data..=) Prelude.<$> deployment,
            ("s3LogBucket" Data..=) Prelude.<$> s3LogBucket,
            ("s3OutputKeyPrefix" Data..=)
              Prelude.<$> s3OutputKeyPrefix,
            ("ssmDocuments" Data..=) Prelude.<$> ssmDocuments
          ]
      )
