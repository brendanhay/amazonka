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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.PostLaunchActions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types.PostLaunchActionsDeploymentType
import Amazonka.MGN.Types.SsmDocument
import qualified Amazonka.Prelude as Prelude

-- | Server participating in Job.
--
-- /See:/ 'newPostLaunchActions' smart constructor.
data PostLaunchActions = PostLaunchActions'
  { -- | Server participating in Job.
    ssmDocuments :: Prelude.Maybe [SsmDocument],
    -- | Server participating in Job.
    deployment :: Prelude.Maybe PostLaunchActionsDeploymentType,
    -- | Server participating in Job.
    s3OutputKeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Server participating in Job.
    s3LogBucket :: Prelude.Maybe Prelude.Text,
    -- | Server participating in Job.
    cloudWatchLogGroupName :: Prelude.Maybe Prelude.Text
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
-- 'ssmDocuments', 'postLaunchActions_ssmDocuments' - Server participating in Job.
--
-- 'deployment', 'postLaunchActions_deployment' - Server participating in Job.
--
-- 's3OutputKeyPrefix', 'postLaunchActions_s3OutputKeyPrefix' - Server participating in Job.
--
-- 's3LogBucket', 'postLaunchActions_s3LogBucket' - Server participating in Job.
--
-- 'cloudWatchLogGroupName', 'postLaunchActions_cloudWatchLogGroupName' - Server participating in Job.
newPostLaunchActions ::
  PostLaunchActions
newPostLaunchActions =
  PostLaunchActions'
    { ssmDocuments = Prelude.Nothing,
      deployment = Prelude.Nothing,
      s3OutputKeyPrefix = Prelude.Nothing,
      s3LogBucket = Prelude.Nothing,
      cloudWatchLogGroupName = Prelude.Nothing
    }

-- | Server participating in Job.
postLaunchActions_ssmDocuments :: Lens.Lens' PostLaunchActions (Prelude.Maybe [SsmDocument])
postLaunchActions_ssmDocuments = Lens.lens (\PostLaunchActions' {ssmDocuments} -> ssmDocuments) (\s@PostLaunchActions' {} a -> s {ssmDocuments = a} :: PostLaunchActions) Prelude.. Lens.mapping Lens.coerced

-- | Server participating in Job.
postLaunchActions_deployment :: Lens.Lens' PostLaunchActions (Prelude.Maybe PostLaunchActionsDeploymentType)
postLaunchActions_deployment = Lens.lens (\PostLaunchActions' {deployment} -> deployment) (\s@PostLaunchActions' {} a -> s {deployment = a} :: PostLaunchActions)

-- | Server participating in Job.
postLaunchActions_s3OutputKeyPrefix :: Lens.Lens' PostLaunchActions (Prelude.Maybe Prelude.Text)
postLaunchActions_s3OutputKeyPrefix = Lens.lens (\PostLaunchActions' {s3OutputKeyPrefix} -> s3OutputKeyPrefix) (\s@PostLaunchActions' {} a -> s {s3OutputKeyPrefix = a} :: PostLaunchActions)

-- | Server participating in Job.
postLaunchActions_s3LogBucket :: Lens.Lens' PostLaunchActions (Prelude.Maybe Prelude.Text)
postLaunchActions_s3LogBucket = Lens.lens (\PostLaunchActions' {s3LogBucket} -> s3LogBucket) (\s@PostLaunchActions' {} a -> s {s3LogBucket = a} :: PostLaunchActions)

-- | Server participating in Job.
postLaunchActions_cloudWatchLogGroupName :: Lens.Lens' PostLaunchActions (Prelude.Maybe Prelude.Text)
postLaunchActions_cloudWatchLogGroupName = Lens.lens (\PostLaunchActions' {cloudWatchLogGroupName} -> cloudWatchLogGroupName) (\s@PostLaunchActions' {} a -> s {cloudWatchLogGroupName = a} :: PostLaunchActions)

instance Core.FromJSON PostLaunchActions where
  parseJSON =
    Core.withObject
      "PostLaunchActions"
      ( \x ->
          PostLaunchActions'
            Prelude.<$> (x Core..:? "ssmDocuments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "deployment")
            Prelude.<*> (x Core..:? "s3OutputKeyPrefix")
            Prelude.<*> (x Core..:? "s3LogBucket")
            Prelude.<*> (x Core..:? "cloudWatchLogGroupName")
      )

instance Prelude.Hashable PostLaunchActions where
  hashWithSalt _salt PostLaunchActions' {..} =
    _salt `Prelude.hashWithSalt` ssmDocuments
      `Prelude.hashWithSalt` deployment
      `Prelude.hashWithSalt` s3OutputKeyPrefix
      `Prelude.hashWithSalt` s3LogBucket
      `Prelude.hashWithSalt` cloudWatchLogGroupName

instance Prelude.NFData PostLaunchActions where
  rnf PostLaunchActions' {..} =
    Prelude.rnf ssmDocuments
      `Prelude.seq` Prelude.rnf deployment
      `Prelude.seq` Prelude.rnf s3OutputKeyPrefix
      `Prelude.seq` Prelude.rnf s3LogBucket
      `Prelude.seq` Prelude.rnf cloudWatchLogGroupName

instance Core.ToJSON PostLaunchActions where
  toJSON PostLaunchActions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ssmDocuments" Core..=) Prelude.<$> ssmDocuments,
            ("deployment" Core..=) Prelude.<$> deployment,
            ("s3OutputKeyPrefix" Core..=)
              Prelude.<$> s3OutputKeyPrefix,
            ("s3LogBucket" Core..=) Prelude.<$> s3LogBucket,
            ("cloudWatchLogGroupName" Core..=)
              Prelude.<$> cloudWatchLogGroupName
          ]
      )
