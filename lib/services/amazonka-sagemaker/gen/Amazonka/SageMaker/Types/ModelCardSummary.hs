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
-- Module      : Amazonka.SageMaker.Types.ModelCardSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelCardSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelCardStatus

-- | A summary of the model card.
--
-- /See:/ 'newModelCardSummary' smart constructor.
data ModelCardSummary = ModelCardSummary'
  { -- | The date and time that the model card was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the model card.
    modelCardName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model card.
    modelCardArn :: Prelude.Text,
    -- | The approval status of the model card within your organization.
    -- Different organizations might have different criteria for model card
    -- review and approval.
    --
    -- -   @Draft@: The model card is a work in progress.
    --
    -- -   @PendingReview@: The model card is pending review.
    --
    -- -   @Approved@: The model card is approved.
    --
    -- -   @Archived@: The model card is archived. No more updates should be
    --     made to the model card, but it can still be exported.
    modelCardStatus :: ModelCardStatus,
    -- | The date and time that the model card was created.
    creationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelCardSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'modelCardSummary_lastModifiedTime' - The date and time that the model card was last modified.
--
-- 'modelCardName', 'modelCardSummary_modelCardName' - The name of the model card.
--
-- 'modelCardArn', 'modelCardSummary_modelCardArn' - The Amazon Resource Name (ARN) of the model card.
--
-- 'modelCardStatus', 'modelCardSummary_modelCardStatus' - The approval status of the model card within your organization.
-- Different organizations might have different criteria for model card
-- review and approval.
--
-- -   @Draft@: The model card is a work in progress.
--
-- -   @PendingReview@: The model card is pending review.
--
-- -   @Approved@: The model card is approved.
--
-- -   @Archived@: The model card is archived. No more updates should be
--     made to the model card, but it can still be exported.
--
-- 'creationTime', 'modelCardSummary_creationTime' - The date and time that the model card was created.
newModelCardSummary ::
  -- | 'modelCardName'
  Prelude.Text ->
  -- | 'modelCardArn'
  Prelude.Text ->
  -- | 'modelCardStatus'
  ModelCardStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  ModelCardSummary
newModelCardSummary
  pModelCardName_
  pModelCardArn_
  pModelCardStatus_
  pCreationTime_ =
    ModelCardSummary'
      { lastModifiedTime =
          Prelude.Nothing,
        modelCardName = pModelCardName_,
        modelCardArn = pModelCardArn_,
        modelCardStatus = pModelCardStatus_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | The date and time that the model card was last modified.
modelCardSummary_lastModifiedTime :: Lens.Lens' ModelCardSummary (Prelude.Maybe Prelude.UTCTime)
modelCardSummary_lastModifiedTime = Lens.lens (\ModelCardSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ModelCardSummary' {} a -> s {lastModifiedTime = a} :: ModelCardSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the model card.
modelCardSummary_modelCardName :: Lens.Lens' ModelCardSummary Prelude.Text
modelCardSummary_modelCardName = Lens.lens (\ModelCardSummary' {modelCardName} -> modelCardName) (\s@ModelCardSummary' {} a -> s {modelCardName = a} :: ModelCardSummary)

-- | The Amazon Resource Name (ARN) of the model card.
modelCardSummary_modelCardArn :: Lens.Lens' ModelCardSummary Prelude.Text
modelCardSummary_modelCardArn = Lens.lens (\ModelCardSummary' {modelCardArn} -> modelCardArn) (\s@ModelCardSummary' {} a -> s {modelCardArn = a} :: ModelCardSummary)

-- | The approval status of the model card within your organization.
-- Different organizations might have different criteria for model card
-- review and approval.
--
-- -   @Draft@: The model card is a work in progress.
--
-- -   @PendingReview@: The model card is pending review.
--
-- -   @Approved@: The model card is approved.
--
-- -   @Archived@: The model card is archived. No more updates should be
--     made to the model card, but it can still be exported.
modelCardSummary_modelCardStatus :: Lens.Lens' ModelCardSummary ModelCardStatus
modelCardSummary_modelCardStatus = Lens.lens (\ModelCardSummary' {modelCardStatus} -> modelCardStatus) (\s@ModelCardSummary' {} a -> s {modelCardStatus = a} :: ModelCardSummary)

-- | The date and time that the model card was created.
modelCardSummary_creationTime :: Lens.Lens' ModelCardSummary Prelude.UTCTime
modelCardSummary_creationTime = Lens.lens (\ModelCardSummary' {creationTime} -> creationTime) (\s@ModelCardSummary' {} a -> s {creationTime = a} :: ModelCardSummary) Prelude.. Data._Time

instance Data.FromJSON ModelCardSummary where
  parseJSON =
    Data.withObject
      "ModelCardSummary"
      ( \x ->
          ModelCardSummary'
            Prelude.<$> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..: "ModelCardName")
            Prelude.<*> (x Data..: "ModelCardArn")
            Prelude.<*> (x Data..: "ModelCardStatus")
            Prelude.<*> (x Data..: "CreationTime")
      )

instance Prelude.Hashable ModelCardSummary where
  hashWithSalt _salt ModelCardSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` modelCardName
      `Prelude.hashWithSalt` modelCardArn
      `Prelude.hashWithSalt` modelCardStatus
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ModelCardSummary where
  rnf ModelCardSummary' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf modelCardName
      `Prelude.seq` Prelude.rnf modelCardArn
      `Prelude.seq` Prelude.rnf modelCardStatus
      `Prelude.seq` Prelude.rnf creationTime
