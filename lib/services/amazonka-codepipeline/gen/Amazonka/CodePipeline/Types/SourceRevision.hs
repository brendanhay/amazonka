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
-- Module      : Amazonka.CodePipeline.Types.SourceRevision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.SourceRevision where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the version (or revision) of a source artifact that
-- initiated a pipeline execution.
--
-- /See:/ 'newSourceRevision' smart constructor.
data SourceRevision = SourceRevision'
  { -- | The system-generated unique ID that identifies the revision number of
    -- the artifact.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | Summary information about the most recent revision of the artifact. For
    -- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
    -- S3 buckets or actions, the user-provided content of a
    -- @codepipeline-artifact-revision-summary@ key specified in the object
    -- metadata.
    revisionSummary :: Prelude.Maybe Prelude.Text,
    -- | The commit ID for the artifact revision. For artifacts stored in GitHub
    -- or AWS CodeCommit repositories, the commit ID is linked to a commit
    -- details page.
    revisionUrl :: Prelude.Maybe Prelude.Text,
    -- | The name of the action that processed the revision to the source
    -- artifact.
    actionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'sourceRevision_revisionId' - The system-generated unique ID that identifies the revision number of
-- the artifact.
--
-- 'revisionSummary', 'sourceRevision_revisionSummary' - Summary information about the most recent revision of the artifact. For
-- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
-- S3 buckets or actions, the user-provided content of a
-- @codepipeline-artifact-revision-summary@ key specified in the object
-- metadata.
--
-- 'revisionUrl', 'sourceRevision_revisionUrl' - The commit ID for the artifact revision. For artifacts stored in GitHub
-- or AWS CodeCommit repositories, the commit ID is linked to a commit
-- details page.
--
-- 'actionName', 'sourceRevision_actionName' - The name of the action that processed the revision to the source
-- artifact.
newSourceRevision ::
  -- | 'actionName'
  Prelude.Text ->
  SourceRevision
newSourceRevision pActionName_ =
  SourceRevision'
    { revisionId = Prelude.Nothing,
      revisionSummary = Prelude.Nothing,
      revisionUrl = Prelude.Nothing,
      actionName = pActionName_
    }

-- | The system-generated unique ID that identifies the revision number of
-- the artifact.
sourceRevision_revisionId :: Lens.Lens' SourceRevision (Prelude.Maybe Prelude.Text)
sourceRevision_revisionId = Lens.lens (\SourceRevision' {revisionId} -> revisionId) (\s@SourceRevision' {} a -> s {revisionId = a} :: SourceRevision)

-- | Summary information about the most recent revision of the artifact. For
-- GitHub and AWS CodeCommit repositories, the commit message. For Amazon
-- S3 buckets or actions, the user-provided content of a
-- @codepipeline-artifact-revision-summary@ key specified in the object
-- metadata.
sourceRevision_revisionSummary :: Lens.Lens' SourceRevision (Prelude.Maybe Prelude.Text)
sourceRevision_revisionSummary = Lens.lens (\SourceRevision' {revisionSummary} -> revisionSummary) (\s@SourceRevision' {} a -> s {revisionSummary = a} :: SourceRevision)

-- | The commit ID for the artifact revision. For artifacts stored in GitHub
-- or AWS CodeCommit repositories, the commit ID is linked to a commit
-- details page.
sourceRevision_revisionUrl :: Lens.Lens' SourceRevision (Prelude.Maybe Prelude.Text)
sourceRevision_revisionUrl = Lens.lens (\SourceRevision' {revisionUrl} -> revisionUrl) (\s@SourceRevision' {} a -> s {revisionUrl = a} :: SourceRevision)

-- | The name of the action that processed the revision to the source
-- artifact.
sourceRevision_actionName :: Lens.Lens' SourceRevision Prelude.Text
sourceRevision_actionName = Lens.lens (\SourceRevision' {actionName} -> actionName) (\s@SourceRevision' {} a -> s {actionName = a} :: SourceRevision)

instance Data.FromJSON SourceRevision where
  parseJSON =
    Data.withObject
      "SourceRevision"
      ( \x ->
          SourceRevision'
            Prelude.<$> (x Data..:? "revisionId")
            Prelude.<*> (x Data..:? "revisionSummary")
            Prelude.<*> (x Data..:? "revisionUrl")
            Prelude.<*> (x Data..: "actionName")
      )

instance Prelude.Hashable SourceRevision where
  hashWithSalt _salt SourceRevision' {..} =
    _salt `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` revisionSummary
      `Prelude.hashWithSalt` revisionUrl
      `Prelude.hashWithSalt` actionName

instance Prelude.NFData SourceRevision where
  rnf SourceRevision' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf revisionSummary
      `Prelude.seq` Prelude.rnf revisionUrl
      `Prelude.seq` Prelude.rnf actionName
