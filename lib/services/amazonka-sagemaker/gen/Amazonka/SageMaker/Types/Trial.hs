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
-- Module      : Amazonka.SageMaker.Types.Trial
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Trial where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MetadataProperties
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.TrialComponentSimpleSummary
import Amazonka.SageMaker.Types.TrialSource
import Amazonka.SageMaker.Types.UserContext

-- | The properties of a trial as returned by the Search API.
--
-- /See:/ 'newTrial' smart constructor.
data Trial = Trial'
  { -- | When the trial was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | A list of the components associated with the trial. For each component,
    -- a summary of the component\'s properties is included.
    trialComponentSummaries :: Prelude.Maybe [TrialComponentSimpleSummary],
    -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    -- | Who created the trial.
    createdBy :: Prelude.Maybe UserContext,
    -- | Who last modified the trial.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the experiment the trial is part of.
    experimentName :: Prelude.Maybe Prelude.Text,
    source :: Prelude.Maybe TrialSource,
    -- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
    -- @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial.
    trialName :: Prelude.Maybe Prelude.Text,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The list of tags that are associated with the trial. You can use Search
    -- API to search on the tags.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Trial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'trial_creationTime' - When the trial was created.
--
-- 'metadataProperties', 'trial_metadataProperties' - Undocumented member.
--
-- 'trialComponentSummaries', 'trial_trialComponentSummaries' - A list of the components associated with the trial. For each component,
-- a summary of the component\'s properties is included.
--
-- 'trialArn', 'trial_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'createdBy', 'trial_createdBy' - Who created the trial.
--
-- 'lastModifiedTime', 'trial_lastModifiedTime' - Who last modified the trial.
--
-- 'experimentName', 'trial_experimentName' - The name of the experiment the trial is part of.
--
-- 'source', 'trial_source' - Undocumented member.
--
-- 'displayName', 'trial_displayName' - The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
--
-- 'trialName', 'trial_trialName' - The name of the trial.
--
-- 'lastModifiedBy', 'trial_lastModifiedBy' - Undocumented member.
--
-- 'tags', 'trial_tags' - The list of tags that are associated with the trial. You can use Search
-- API to search on the tags.
newTrial ::
  Trial
newTrial =
  Trial'
    { creationTime = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      trialComponentSummaries = Prelude.Nothing,
      trialArn = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      source = Prelude.Nothing,
      displayName = Prelude.Nothing,
      trialName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | When the trial was created.
trial_creationTime :: Lens.Lens' Trial (Prelude.Maybe Prelude.UTCTime)
trial_creationTime = Lens.lens (\Trial' {creationTime} -> creationTime) (\s@Trial' {} a -> s {creationTime = a} :: Trial) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
trial_metadataProperties :: Lens.Lens' Trial (Prelude.Maybe MetadataProperties)
trial_metadataProperties = Lens.lens (\Trial' {metadataProperties} -> metadataProperties) (\s@Trial' {} a -> s {metadataProperties = a} :: Trial)

-- | A list of the components associated with the trial. For each component,
-- a summary of the component\'s properties is included.
trial_trialComponentSummaries :: Lens.Lens' Trial (Prelude.Maybe [TrialComponentSimpleSummary])
trial_trialComponentSummaries = Lens.lens (\Trial' {trialComponentSummaries} -> trialComponentSummaries) (\s@Trial' {} a -> s {trialComponentSummaries = a} :: Trial) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the trial.
trial_trialArn :: Lens.Lens' Trial (Prelude.Maybe Prelude.Text)
trial_trialArn = Lens.lens (\Trial' {trialArn} -> trialArn) (\s@Trial' {} a -> s {trialArn = a} :: Trial)

-- | Who created the trial.
trial_createdBy :: Lens.Lens' Trial (Prelude.Maybe UserContext)
trial_createdBy = Lens.lens (\Trial' {createdBy} -> createdBy) (\s@Trial' {} a -> s {createdBy = a} :: Trial)

-- | Who last modified the trial.
trial_lastModifiedTime :: Lens.Lens' Trial (Prelude.Maybe Prelude.UTCTime)
trial_lastModifiedTime = Lens.lens (\Trial' {lastModifiedTime} -> lastModifiedTime) (\s@Trial' {} a -> s {lastModifiedTime = a} :: Trial) Prelude.. Lens.mapping Core._Time

-- | The name of the experiment the trial is part of.
trial_experimentName :: Lens.Lens' Trial (Prelude.Maybe Prelude.Text)
trial_experimentName = Lens.lens (\Trial' {experimentName} -> experimentName) (\s@Trial' {} a -> s {experimentName = a} :: Trial)

-- | Undocumented member.
trial_source :: Lens.Lens' Trial (Prelude.Maybe TrialSource)
trial_source = Lens.lens (\Trial' {source} -> source) (\s@Trial' {} a -> s {source = a} :: Trial)

-- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
trial_displayName :: Lens.Lens' Trial (Prelude.Maybe Prelude.Text)
trial_displayName = Lens.lens (\Trial' {displayName} -> displayName) (\s@Trial' {} a -> s {displayName = a} :: Trial)

-- | The name of the trial.
trial_trialName :: Lens.Lens' Trial (Prelude.Maybe Prelude.Text)
trial_trialName = Lens.lens (\Trial' {trialName} -> trialName) (\s@Trial' {} a -> s {trialName = a} :: Trial)

-- | Undocumented member.
trial_lastModifiedBy :: Lens.Lens' Trial (Prelude.Maybe UserContext)
trial_lastModifiedBy = Lens.lens (\Trial' {lastModifiedBy} -> lastModifiedBy) (\s@Trial' {} a -> s {lastModifiedBy = a} :: Trial)

-- | The list of tags that are associated with the trial. You can use Search
-- API to search on the tags.
trial_tags :: Lens.Lens' Trial (Prelude.Maybe [Tag])
trial_tags = Lens.lens (\Trial' {tags} -> tags) (\s@Trial' {} a -> s {tags = a} :: Trial) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Trial where
  parseJSON =
    Core.withObject
      "Trial"
      ( \x ->
          Trial'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "MetadataProperties")
            Prelude.<*> ( x Core..:? "TrialComponentSummaries"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TrialArn")
            Prelude.<*> (x Core..:? "CreatedBy")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "ExperimentName")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "TrialName")
            Prelude.<*> (x Core..:? "LastModifiedBy")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Trial where
  hashWithSalt _salt Trial' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` metadataProperties
      `Prelude.hashWithSalt` trialComponentSummaries
      `Prelude.hashWithSalt` trialArn
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` experimentName
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` trialName
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Trial where
  rnf Trial' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf metadataProperties
      `Prelude.seq` Prelude.rnf trialComponentSummaries
      `Prelude.seq` Prelude.rnf trialArn
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf experimentName
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf trialName
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf tags
