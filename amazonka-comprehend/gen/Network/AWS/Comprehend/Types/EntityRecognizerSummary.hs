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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerSummary where

import Network.AWS.Comprehend.Types.ModelStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the information about an entity recognizer and its versions.
--
-- /See:/ 'newEntityRecognizerSummary' smart constructor.
data EntityRecognizerSummary = EntityRecognizerSummary'
  { -- | The time that the latest entity recognizer version was submitted for
    -- processing.
    latestVersionCreatedAt :: Prelude.Maybe Core.POSIX,
    -- | The name that you assigned the entity recognizer.
    recognizerName :: Prelude.Maybe Prelude.Text,
    -- | The version name you assigned to the latest entity recognizer version.
    latestVersionName :: Prelude.Maybe Prelude.Text,
    -- | The number of versions you created.
    numberOfVersions :: Prelude.Maybe Prelude.Int,
    -- | Provides the status of the latest entity recognizer version.
    latestVersionStatus :: Prelude.Maybe ModelStatus
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
-- 'latestVersionCreatedAt', 'entityRecognizerSummary_latestVersionCreatedAt' - The time that the latest entity recognizer version was submitted for
-- processing.
--
-- 'recognizerName', 'entityRecognizerSummary_recognizerName' - The name that you assigned the entity recognizer.
--
-- 'latestVersionName', 'entityRecognizerSummary_latestVersionName' - The version name you assigned to the latest entity recognizer version.
--
-- 'numberOfVersions', 'entityRecognizerSummary_numberOfVersions' - The number of versions you created.
--
-- 'latestVersionStatus', 'entityRecognizerSummary_latestVersionStatus' - Provides the status of the latest entity recognizer version.
newEntityRecognizerSummary ::
  EntityRecognizerSummary
newEntityRecognizerSummary =
  EntityRecognizerSummary'
    { latestVersionCreatedAt =
        Prelude.Nothing,
      recognizerName = Prelude.Nothing,
      latestVersionName = Prelude.Nothing,
      numberOfVersions = Prelude.Nothing,
      latestVersionStatus = Prelude.Nothing
    }

-- | The time that the latest entity recognizer version was submitted for
-- processing.
entityRecognizerSummary_latestVersionCreatedAt :: Lens.Lens' EntityRecognizerSummary (Prelude.Maybe Prelude.UTCTime)
entityRecognizerSummary_latestVersionCreatedAt = Lens.lens (\EntityRecognizerSummary' {latestVersionCreatedAt} -> latestVersionCreatedAt) (\s@EntityRecognizerSummary' {} a -> s {latestVersionCreatedAt = a} :: EntityRecognizerSummary) Prelude.. Lens.mapping Core._Time

-- | The name that you assigned the entity recognizer.
entityRecognizerSummary_recognizerName :: Lens.Lens' EntityRecognizerSummary (Prelude.Maybe Prelude.Text)
entityRecognizerSummary_recognizerName = Lens.lens (\EntityRecognizerSummary' {recognizerName} -> recognizerName) (\s@EntityRecognizerSummary' {} a -> s {recognizerName = a} :: EntityRecognizerSummary)

-- | The version name you assigned to the latest entity recognizer version.
entityRecognizerSummary_latestVersionName :: Lens.Lens' EntityRecognizerSummary (Prelude.Maybe Prelude.Text)
entityRecognizerSummary_latestVersionName = Lens.lens (\EntityRecognizerSummary' {latestVersionName} -> latestVersionName) (\s@EntityRecognizerSummary' {} a -> s {latestVersionName = a} :: EntityRecognizerSummary)

-- | The number of versions you created.
entityRecognizerSummary_numberOfVersions :: Lens.Lens' EntityRecognizerSummary (Prelude.Maybe Prelude.Int)
entityRecognizerSummary_numberOfVersions = Lens.lens (\EntityRecognizerSummary' {numberOfVersions} -> numberOfVersions) (\s@EntityRecognizerSummary' {} a -> s {numberOfVersions = a} :: EntityRecognizerSummary)

-- | Provides the status of the latest entity recognizer version.
entityRecognizerSummary_latestVersionStatus :: Lens.Lens' EntityRecognizerSummary (Prelude.Maybe ModelStatus)
entityRecognizerSummary_latestVersionStatus = Lens.lens (\EntityRecognizerSummary' {latestVersionStatus} -> latestVersionStatus) (\s@EntityRecognizerSummary' {} a -> s {latestVersionStatus = a} :: EntityRecognizerSummary)

instance Core.FromJSON EntityRecognizerSummary where
  parseJSON =
    Core.withObject
      "EntityRecognizerSummary"
      ( \x ->
          EntityRecognizerSummary'
            Prelude.<$> (x Core..:? "LatestVersionCreatedAt")
            Prelude.<*> (x Core..:? "RecognizerName")
            Prelude.<*> (x Core..:? "LatestVersionName")
            Prelude.<*> (x Core..:? "NumberOfVersions")
            Prelude.<*> (x Core..:? "LatestVersionStatus")
      )

instance Prelude.Hashable EntityRecognizerSummary

instance Prelude.NFData EntityRecognizerSummary
