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
-- Module      : Amazonka.Comprehend.Types.DocumentClassifierSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentClassifierSummary where

import Amazonka.Comprehend.Types.ModelStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes information about a document classifier and its versions.
--
-- /See:/ 'newDocumentClassifierSummary' smart constructor.
data DocumentClassifierSummary = DocumentClassifierSummary'
  { -- | The number of versions you created.
    numberOfVersions :: Prelude.Maybe Prelude.Int,
    -- | The version name you assigned to the latest document classifier version.
    latestVersionName :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of the latest document classifier version.
    latestVersionStatus :: Prelude.Maybe ModelStatus,
    -- | The name that you assigned the document classifier.
    documentClassifierName :: Prelude.Maybe Prelude.Text,
    -- | The time that the latest document classifier version was submitted for
    -- processing.
    latestVersionCreatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentClassifierSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfVersions', 'documentClassifierSummary_numberOfVersions' - The number of versions you created.
--
-- 'latestVersionName', 'documentClassifierSummary_latestVersionName' - The version name you assigned to the latest document classifier version.
--
-- 'latestVersionStatus', 'documentClassifierSummary_latestVersionStatus' - Provides the status of the latest document classifier version.
--
-- 'documentClassifierName', 'documentClassifierSummary_documentClassifierName' - The name that you assigned the document classifier.
--
-- 'latestVersionCreatedAt', 'documentClassifierSummary_latestVersionCreatedAt' - The time that the latest document classifier version was submitted for
-- processing.
newDocumentClassifierSummary ::
  DocumentClassifierSummary
newDocumentClassifierSummary =
  DocumentClassifierSummary'
    { numberOfVersions =
        Prelude.Nothing,
      latestVersionName = Prelude.Nothing,
      latestVersionStatus = Prelude.Nothing,
      documentClassifierName = Prelude.Nothing,
      latestVersionCreatedAt = Prelude.Nothing
    }

-- | The number of versions you created.
documentClassifierSummary_numberOfVersions :: Lens.Lens' DocumentClassifierSummary (Prelude.Maybe Prelude.Int)
documentClassifierSummary_numberOfVersions = Lens.lens (\DocumentClassifierSummary' {numberOfVersions} -> numberOfVersions) (\s@DocumentClassifierSummary' {} a -> s {numberOfVersions = a} :: DocumentClassifierSummary)

-- | The version name you assigned to the latest document classifier version.
documentClassifierSummary_latestVersionName :: Lens.Lens' DocumentClassifierSummary (Prelude.Maybe Prelude.Text)
documentClassifierSummary_latestVersionName = Lens.lens (\DocumentClassifierSummary' {latestVersionName} -> latestVersionName) (\s@DocumentClassifierSummary' {} a -> s {latestVersionName = a} :: DocumentClassifierSummary)

-- | Provides the status of the latest document classifier version.
documentClassifierSummary_latestVersionStatus :: Lens.Lens' DocumentClassifierSummary (Prelude.Maybe ModelStatus)
documentClassifierSummary_latestVersionStatus = Lens.lens (\DocumentClassifierSummary' {latestVersionStatus} -> latestVersionStatus) (\s@DocumentClassifierSummary' {} a -> s {latestVersionStatus = a} :: DocumentClassifierSummary)

-- | The name that you assigned the document classifier.
documentClassifierSummary_documentClassifierName :: Lens.Lens' DocumentClassifierSummary (Prelude.Maybe Prelude.Text)
documentClassifierSummary_documentClassifierName = Lens.lens (\DocumentClassifierSummary' {documentClassifierName} -> documentClassifierName) (\s@DocumentClassifierSummary' {} a -> s {documentClassifierName = a} :: DocumentClassifierSummary)

-- | The time that the latest document classifier version was submitted for
-- processing.
documentClassifierSummary_latestVersionCreatedAt :: Lens.Lens' DocumentClassifierSummary (Prelude.Maybe Prelude.UTCTime)
documentClassifierSummary_latestVersionCreatedAt = Lens.lens (\DocumentClassifierSummary' {latestVersionCreatedAt} -> latestVersionCreatedAt) (\s@DocumentClassifierSummary' {} a -> s {latestVersionCreatedAt = a} :: DocumentClassifierSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DocumentClassifierSummary where
  parseJSON =
    Data.withObject
      "DocumentClassifierSummary"
      ( \x ->
          DocumentClassifierSummary'
            Prelude.<$> (x Data..:? "NumberOfVersions")
            Prelude.<*> (x Data..:? "LatestVersionName")
            Prelude.<*> (x Data..:? "LatestVersionStatus")
            Prelude.<*> (x Data..:? "DocumentClassifierName")
            Prelude.<*> (x Data..:? "LatestVersionCreatedAt")
      )

instance Prelude.Hashable DocumentClassifierSummary where
  hashWithSalt _salt DocumentClassifierSummary' {..} =
    _salt `Prelude.hashWithSalt` numberOfVersions
      `Prelude.hashWithSalt` latestVersionName
      `Prelude.hashWithSalt` latestVersionStatus
      `Prelude.hashWithSalt` documentClassifierName
      `Prelude.hashWithSalt` latestVersionCreatedAt

instance Prelude.NFData DocumentClassifierSummary where
  rnf DocumentClassifierSummary' {..} =
    Prelude.rnf numberOfVersions
      `Prelude.seq` Prelude.rnf latestVersionName
      `Prelude.seq` Prelude.rnf latestVersionStatus
      `Prelude.seq` Prelude.rnf documentClassifierName
      `Prelude.seq` Prelude.rnf latestVersionCreatedAt
