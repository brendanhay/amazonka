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
-- Module      : Network.AWS.SageMaker.Types.Trial
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Trial where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MetadataProperties
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
import Network.AWS.SageMaker.Types.TrialSource
import Network.AWS.SageMaker.Types.UserContext

-- | The properties of a trial as returned by the Search API.
--
-- /See:/ 'newTrial' smart constructor.
data Trial = Trial'
  { -- | The Amazon Resource Name (ARN) of the trial.
    trialArn :: Prelude.Maybe Prelude.Text,
    metadataProperties :: Prelude.Maybe MetadataProperties,
    -- | When the trial was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    source :: Prelude.Maybe TrialSource,
    -- | A list of the components associated with the trial. For each component,
    -- a summary of the component\'s properties is included.
    trialComponentSummaries :: Prelude.Maybe [TrialComponentSimpleSummary],
    -- | The list of tags that are associated with the trial. You can use Search
    -- API to search on the tags.
    tags :: Prelude.Maybe [Tag],
    -- | Who last modified the trial.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the experiment the trial is part of.
    experimentName :: Prelude.Maybe Prelude.Text,
    createdBy :: Prelude.Maybe UserContext,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
    -- @TrialName@ is displayed.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The name of the trial.
    trialName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Trial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trialArn', 'trial_trialArn' - The Amazon Resource Name (ARN) of the trial.
--
-- 'metadataProperties', 'trial_metadataProperties' - Undocumented member.
--
-- 'creationTime', 'trial_creationTime' - When the trial was created.
--
-- 'source', 'trial_source' - Undocumented member.
--
-- 'trialComponentSummaries', 'trial_trialComponentSummaries' - A list of the components associated with the trial. For each component,
-- a summary of the component\'s properties is included.
--
-- 'tags', 'trial_tags' - The list of tags that are associated with the trial. You can use Search
-- API to search on the tags.
--
-- 'lastModifiedTime', 'trial_lastModifiedTime' - Who last modified the trial.
--
-- 'experimentName', 'trial_experimentName' - The name of the experiment the trial is part of.
--
-- 'createdBy', 'trial_createdBy' - Undocumented member.
--
-- 'lastModifiedBy', 'trial_lastModifiedBy' - Undocumented member.
--
-- 'displayName', 'trial_displayName' - The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
--
-- 'trialName', 'trial_trialName' - The name of the trial.
newTrial ::
  Trial
newTrial =
  Trial'
    { trialArn = Prelude.Nothing,
      metadataProperties = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      source = Prelude.Nothing,
      trialComponentSummaries = Prelude.Nothing,
      tags = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      displayName = Prelude.Nothing,
      trialName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the trial.
trial_trialArn :: Lens.Lens' Trial (Prelude.Maybe Prelude.Text)
trial_trialArn = Lens.lens (\Trial' {trialArn} -> trialArn) (\s@Trial' {} a -> s {trialArn = a} :: Trial)

-- | Undocumented member.
trial_metadataProperties :: Lens.Lens' Trial (Prelude.Maybe MetadataProperties)
trial_metadataProperties = Lens.lens (\Trial' {metadataProperties} -> metadataProperties) (\s@Trial' {} a -> s {metadataProperties = a} :: Trial)

-- | When the trial was created.
trial_creationTime :: Lens.Lens' Trial (Prelude.Maybe Prelude.UTCTime)
trial_creationTime = Lens.lens (\Trial' {creationTime} -> creationTime) (\s@Trial' {} a -> s {creationTime = a} :: Trial) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
trial_source :: Lens.Lens' Trial (Prelude.Maybe TrialSource)
trial_source = Lens.lens (\Trial' {source} -> source) (\s@Trial' {} a -> s {source = a} :: Trial)

-- | A list of the components associated with the trial. For each component,
-- a summary of the component\'s properties is included.
trial_trialComponentSummaries :: Lens.Lens' Trial (Prelude.Maybe [TrialComponentSimpleSummary])
trial_trialComponentSummaries = Lens.lens (\Trial' {trialComponentSummaries} -> trialComponentSummaries) (\s@Trial' {} a -> s {trialComponentSummaries = a} :: Trial) Prelude.. Lens.mapping Prelude._Coerce

-- | The list of tags that are associated with the trial. You can use Search
-- API to search on the tags.
trial_tags :: Lens.Lens' Trial (Prelude.Maybe [Tag])
trial_tags = Lens.lens (\Trial' {tags} -> tags) (\s@Trial' {} a -> s {tags = a} :: Trial) Prelude.. Lens.mapping Prelude._Coerce

-- | Who last modified the trial.
trial_lastModifiedTime :: Lens.Lens' Trial (Prelude.Maybe Prelude.UTCTime)
trial_lastModifiedTime = Lens.lens (\Trial' {lastModifiedTime} -> lastModifiedTime) (\s@Trial' {} a -> s {lastModifiedTime = a} :: Trial) Prelude.. Lens.mapping Prelude._Time

-- | The name of the experiment the trial is part of.
trial_experimentName :: Lens.Lens' Trial (Prelude.Maybe Prelude.Text)
trial_experimentName = Lens.lens (\Trial' {experimentName} -> experimentName) (\s@Trial' {} a -> s {experimentName = a} :: Trial)

-- | Undocumented member.
trial_createdBy :: Lens.Lens' Trial (Prelude.Maybe UserContext)
trial_createdBy = Lens.lens (\Trial' {createdBy} -> createdBy) (\s@Trial' {} a -> s {createdBy = a} :: Trial)

-- | Undocumented member.
trial_lastModifiedBy :: Lens.Lens' Trial (Prelude.Maybe UserContext)
trial_lastModifiedBy = Lens.lens (\Trial' {lastModifiedBy} -> lastModifiedBy) (\s@Trial' {} a -> s {lastModifiedBy = a} :: Trial)

-- | The name of the trial as displayed. If @DisplayName@ isn\'t specified,
-- @TrialName@ is displayed.
trial_displayName :: Lens.Lens' Trial (Prelude.Maybe Prelude.Text)
trial_displayName = Lens.lens (\Trial' {displayName} -> displayName) (\s@Trial' {} a -> s {displayName = a} :: Trial)

-- | The name of the trial.
trial_trialName :: Lens.Lens' Trial (Prelude.Maybe Prelude.Text)
trial_trialName = Lens.lens (\Trial' {trialName} -> trialName) (\s@Trial' {} a -> s {trialName = a} :: Trial)

instance Prelude.FromJSON Trial where
  parseJSON =
    Prelude.withObject
      "Trial"
      ( \x ->
          Trial'
            Prelude.<$> (x Prelude..:? "TrialArn")
            Prelude.<*> (x Prelude..:? "MetadataProperties")
            Prelude.<*> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "Source")
            Prelude.<*> ( x Prelude..:? "TrialComponentSummaries"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "ExperimentName")
            Prelude.<*> (x Prelude..:? "CreatedBy")
            Prelude.<*> (x Prelude..:? "LastModifiedBy")
            Prelude.<*> (x Prelude..:? "DisplayName")
            Prelude.<*> (x Prelude..:? "TrialName")
      )

instance Prelude.Hashable Trial

instance Prelude.NFData Trial
