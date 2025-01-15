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
-- Module      : Amazonka.SageMaker.Types.ModelCardVersionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelCardVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelCardStatus

-- | A summary of a specific version of the model card.
--
-- /See:/ 'newModelCardVersionSummary' smart constructor.
data ModelCardVersionSummary = ModelCardVersionSummary'
  { -- | The time date and time that the model card version was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the model card.
    modelCardName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model card.
    modelCardArn :: Prelude.Text,
    -- | The approval status of the model card version within your organization.
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
    -- | A version of the model card.
    modelCardVersion :: Prelude.Int,
    -- | The date and time that the model card version was created.
    creationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelCardVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'modelCardVersionSummary_lastModifiedTime' - The time date and time that the model card version was last modified.
--
-- 'modelCardName', 'modelCardVersionSummary_modelCardName' - The name of the model card.
--
-- 'modelCardArn', 'modelCardVersionSummary_modelCardArn' - The Amazon Resource Name (ARN) of the model card.
--
-- 'modelCardStatus', 'modelCardVersionSummary_modelCardStatus' - The approval status of the model card version within your organization.
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
-- 'modelCardVersion', 'modelCardVersionSummary_modelCardVersion' - A version of the model card.
--
-- 'creationTime', 'modelCardVersionSummary_creationTime' - The date and time that the model card version was created.
newModelCardVersionSummary ::
  -- | 'modelCardName'
  Prelude.Text ->
  -- | 'modelCardArn'
  Prelude.Text ->
  -- | 'modelCardStatus'
  ModelCardStatus ->
  -- | 'modelCardVersion'
  Prelude.Int ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  ModelCardVersionSummary
newModelCardVersionSummary
  pModelCardName_
  pModelCardArn_
  pModelCardStatus_
  pModelCardVersion_
  pCreationTime_ =
    ModelCardVersionSummary'
      { lastModifiedTime =
          Prelude.Nothing,
        modelCardName = pModelCardName_,
        modelCardArn = pModelCardArn_,
        modelCardStatus = pModelCardStatus_,
        modelCardVersion = pModelCardVersion_,
        creationTime = Data._Time Lens.# pCreationTime_
      }

-- | The time date and time that the model card version was last modified.
modelCardVersionSummary_lastModifiedTime :: Lens.Lens' ModelCardVersionSummary (Prelude.Maybe Prelude.UTCTime)
modelCardVersionSummary_lastModifiedTime = Lens.lens (\ModelCardVersionSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ModelCardVersionSummary' {} a -> s {lastModifiedTime = a} :: ModelCardVersionSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the model card.
modelCardVersionSummary_modelCardName :: Lens.Lens' ModelCardVersionSummary Prelude.Text
modelCardVersionSummary_modelCardName = Lens.lens (\ModelCardVersionSummary' {modelCardName} -> modelCardName) (\s@ModelCardVersionSummary' {} a -> s {modelCardName = a} :: ModelCardVersionSummary)

-- | The Amazon Resource Name (ARN) of the model card.
modelCardVersionSummary_modelCardArn :: Lens.Lens' ModelCardVersionSummary Prelude.Text
modelCardVersionSummary_modelCardArn = Lens.lens (\ModelCardVersionSummary' {modelCardArn} -> modelCardArn) (\s@ModelCardVersionSummary' {} a -> s {modelCardArn = a} :: ModelCardVersionSummary)

-- | The approval status of the model card version within your organization.
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
modelCardVersionSummary_modelCardStatus :: Lens.Lens' ModelCardVersionSummary ModelCardStatus
modelCardVersionSummary_modelCardStatus = Lens.lens (\ModelCardVersionSummary' {modelCardStatus} -> modelCardStatus) (\s@ModelCardVersionSummary' {} a -> s {modelCardStatus = a} :: ModelCardVersionSummary)

-- | A version of the model card.
modelCardVersionSummary_modelCardVersion :: Lens.Lens' ModelCardVersionSummary Prelude.Int
modelCardVersionSummary_modelCardVersion = Lens.lens (\ModelCardVersionSummary' {modelCardVersion} -> modelCardVersion) (\s@ModelCardVersionSummary' {} a -> s {modelCardVersion = a} :: ModelCardVersionSummary)

-- | The date and time that the model card version was created.
modelCardVersionSummary_creationTime :: Lens.Lens' ModelCardVersionSummary Prelude.UTCTime
modelCardVersionSummary_creationTime = Lens.lens (\ModelCardVersionSummary' {creationTime} -> creationTime) (\s@ModelCardVersionSummary' {} a -> s {creationTime = a} :: ModelCardVersionSummary) Prelude.. Data._Time

instance Data.FromJSON ModelCardVersionSummary where
  parseJSON =
    Data.withObject
      "ModelCardVersionSummary"
      ( \x ->
          ModelCardVersionSummary'
            Prelude.<$> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..: "ModelCardName")
            Prelude.<*> (x Data..: "ModelCardArn")
            Prelude.<*> (x Data..: "ModelCardStatus")
            Prelude.<*> (x Data..: "ModelCardVersion")
            Prelude.<*> (x Data..: "CreationTime")
      )

instance Prelude.Hashable ModelCardVersionSummary where
  hashWithSalt _salt ModelCardVersionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` modelCardName
      `Prelude.hashWithSalt` modelCardArn
      `Prelude.hashWithSalt` modelCardStatus
      `Prelude.hashWithSalt` modelCardVersion
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ModelCardVersionSummary where
  rnf ModelCardVersionSummary' {..} =
    Prelude.rnf lastModifiedTime `Prelude.seq`
      Prelude.rnf modelCardName `Prelude.seq`
        Prelude.rnf modelCardArn `Prelude.seq`
          Prelude.rnf modelCardStatus `Prelude.seq`
            Prelude.rnf modelCardVersion `Prelude.seq`
              Prelude.rnf creationTime
