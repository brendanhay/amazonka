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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerSummary where

import Amazonka.Comprehend.Types.ModelStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the information about an entity recognizer and its versions.
--
-- /See:/ 'newEntityRecognizerSummary' smart constructor.
data EntityRecognizerSummary = EntityRecognizerSummary'
  { -- | The number of versions you created.
    numberOfVersions :: Prelude.Maybe Prelude.Int,
    -- | The version name you assigned to the latest entity recognizer version.
    latestVersionName :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned the entity recognizer.
    recognizerName :: Prelude.Maybe Prelude.Text,
    -- | Provides the status of the latest entity recognizer version.
    latestVersionStatus :: Prelude.Maybe ModelStatus,
    -- | The time that the latest entity recognizer version was submitted for
    -- processing.
    latestVersionCreatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfVersions', 'entityRecognizerSummary_numberOfVersions' - The number of versions you created.
--
-- 'latestVersionName', 'entityRecognizerSummary_latestVersionName' - The version name you assigned to the latest entity recognizer version.
--
-- 'recognizerName', 'entityRecognizerSummary_recognizerName' - The name that you assigned the entity recognizer.
--
-- 'latestVersionStatus', 'entityRecognizerSummary_latestVersionStatus' - Provides the status of the latest entity recognizer version.
--
-- 'latestVersionCreatedAt', 'entityRecognizerSummary_latestVersionCreatedAt' - The time that the latest entity recognizer version was submitted for
-- processing.
newEntityRecognizerSummary ::
  EntityRecognizerSummary
newEntityRecognizerSummary =
  EntityRecognizerSummary'
    { numberOfVersions =
        Prelude.Nothing,
      latestVersionName = Prelude.Nothing,
      recognizerName = Prelude.Nothing,
      latestVersionStatus = Prelude.Nothing,
      latestVersionCreatedAt = Prelude.Nothing
    }

-- | The number of versions you created.
entityRecognizerSummary_numberOfVersions :: Lens.Lens' EntityRecognizerSummary (Prelude.Maybe Prelude.Int)
entityRecognizerSummary_numberOfVersions = Lens.lens (\EntityRecognizerSummary' {numberOfVersions} -> numberOfVersions) (\s@EntityRecognizerSummary' {} a -> s {numberOfVersions = a} :: EntityRecognizerSummary)

-- | The version name you assigned to the latest entity recognizer version.
entityRecognizerSummary_latestVersionName :: Lens.Lens' EntityRecognizerSummary (Prelude.Maybe Prelude.Text)
entityRecognizerSummary_latestVersionName = Lens.lens (\EntityRecognizerSummary' {latestVersionName} -> latestVersionName) (\s@EntityRecognizerSummary' {} a -> s {latestVersionName = a} :: EntityRecognizerSummary)

-- | The name that you assigned the entity recognizer.
entityRecognizerSummary_recognizerName :: Lens.Lens' EntityRecognizerSummary (Prelude.Maybe Prelude.Text)
entityRecognizerSummary_recognizerName = Lens.lens (\EntityRecognizerSummary' {recognizerName} -> recognizerName) (\s@EntityRecognizerSummary' {} a -> s {recognizerName = a} :: EntityRecognizerSummary)

-- | Provides the status of the latest entity recognizer version.
entityRecognizerSummary_latestVersionStatus :: Lens.Lens' EntityRecognizerSummary (Prelude.Maybe ModelStatus)
entityRecognizerSummary_latestVersionStatus = Lens.lens (\EntityRecognizerSummary' {latestVersionStatus} -> latestVersionStatus) (\s@EntityRecognizerSummary' {} a -> s {latestVersionStatus = a} :: EntityRecognizerSummary)

-- | The time that the latest entity recognizer version was submitted for
-- processing.
entityRecognizerSummary_latestVersionCreatedAt :: Lens.Lens' EntityRecognizerSummary (Prelude.Maybe Prelude.UTCTime)
entityRecognizerSummary_latestVersionCreatedAt = Lens.lens (\EntityRecognizerSummary' {latestVersionCreatedAt} -> latestVersionCreatedAt) (\s@EntityRecognizerSummary' {} a -> s {latestVersionCreatedAt = a} :: EntityRecognizerSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON EntityRecognizerSummary where
  parseJSON =
    Data.withObject
      "EntityRecognizerSummary"
      ( \x ->
          EntityRecognizerSummary'
            Prelude.<$> (x Data..:? "NumberOfVersions")
            Prelude.<*> (x Data..:? "LatestVersionName")
            Prelude.<*> (x Data..:? "RecognizerName")
            Prelude.<*> (x Data..:? "LatestVersionStatus")
            Prelude.<*> (x Data..:? "LatestVersionCreatedAt")
      )

instance Prelude.Hashable EntityRecognizerSummary where
  hashWithSalt _salt EntityRecognizerSummary' {..} =
    _salt `Prelude.hashWithSalt` numberOfVersions
      `Prelude.hashWithSalt` latestVersionName
      `Prelude.hashWithSalt` recognizerName
      `Prelude.hashWithSalt` latestVersionStatus
      `Prelude.hashWithSalt` latestVersionCreatedAt

instance Prelude.NFData EntityRecognizerSummary where
  rnf EntityRecognizerSummary' {..} =
    Prelude.rnf numberOfVersions
      `Prelude.seq` Prelude.rnf latestVersionName
      `Prelude.seq` Prelude.rnf recognizerName
      `Prelude.seq` Prelude.rnf latestVersionStatus
      `Prelude.seq` Prelude.rnf latestVersionCreatedAt
