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
-- Module      : Amazonka.SageMaker.Types.ModelCard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelCard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelCardSecurityConfig
import Amazonka.SageMaker.Types.ModelCardStatus
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.UserContext

-- | An Amazon SageMaker Model Card.
--
-- /See:/ 'newModelCard' smart constructor.
data ModelCard = ModelCard'
  { -- | The content of the model card. Content uses the
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-api-json-schema.html model card JSON schema>
    -- and provided as a string.
    content :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    createdBy :: Prelude.Maybe UserContext,
    -- | The date and time that the model card was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | The date and time that the model card was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the model card.
    modelCardArn :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the model card.
    modelCardName :: Prelude.Maybe Prelude.Text,
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
    modelCardStatus :: Prelude.Maybe ModelCardStatus,
    -- | The version of the model card.
    modelCardVersion :: Prelude.Maybe Prelude.Int,
    -- | The unique name (ID) of the model.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | The risk rating of the model. Different organizations might have
    -- different criteria for model card risk ratings. For more information,
    -- see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-risk-rating.html Risk ratings>.
    riskRating :: Prelude.Maybe Prelude.Text,
    -- | The security configuration used to protect model card data.
    securityConfig :: Prelude.Maybe ModelCardSecurityConfig,
    -- | Key-value pairs used to manage metadata for the model card.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelCard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'modelCard_content' - The content of the model card. Content uses the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-api-json-schema.html model card JSON schema>
-- and provided as a string.
--
-- 'createdBy', 'modelCard_createdBy' - Undocumented member.
--
-- 'creationTime', 'modelCard_creationTime' - The date and time that the model card was created.
--
-- 'lastModifiedBy', 'modelCard_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'modelCard_lastModifiedTime' - The date and time that the model card was last modified.
--
-- 'modelCardArn', 'modelCard_modelCardArn' - The Amazon Resource Name (ARN) of the model card.
--
-- 'modelCardName', 'modelCard_modelCardName' - The unique name of the model card.
--
-- 'modelCardStatus', 'modelCard_modelCardStatus' - The approval status of the model card within your organization.
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
-- 'modelCardVersion', 'modelCard_modelCardVersion' - The version of the model card.
--
-- 'modelId', 'modelCard_modelId' - The unique name (ID) of the model.
--
-- 'riskRating', 'modelCard_riskRating' - The risk rating of the model. Different organizations might have
-- different criteria for model card risk ratings. For more information,
-- see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-risk-rating.html Risk ratings>.
--
-- 'securityConfig', 'modelCard_securityConfig' - The security configuration used to protect model card data.
--
-- 'tags', 'modelCard_tags' - Key-value pairs used to manage metadata for the model card.
newModelCard ::
  ModelCard
newModelCard =
  ModelCard'
    { content = Prelude.Nothing,
      createdBy = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      modelCardArn = Prelude.Nothing,
      modelCardName = Prelude.Nothing,
      modelCardStatus = Prelude.Nothing,
      modelCardVersion = Prelude.Nothing,
      modelId = Prelude.Nothing,
      riskRating = Prelude.Nothing,
      securityConfig = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The content of the model card. Content uses the
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-api-json-schema.html model card JSON schema>
-- and provided as a string.
modelCard_content :: Lens.Lens' ModelCard (Prelude.Maybe Prelude.Text)
modelCard_content = Lens.lens (\ModelCard' {content} -> content) (\s@ModelCard' {} a -> s {content = a} :: ModelCard) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
modelCard_createdBy :: Lens.Lens' ModelCard (Prelude.Maybe UserContext)
modelCard_createdBy = Lens.lens (\ModelCard' {createdBy} -> createdBy) (\s@ModelCard' {} a -> s {createdBy = a} :: ModelCard)

-- | The date and time that the model card was created.
modelCard_creationTime :: Lens.Lens' ModelCard (Prelude.Maybe Prelude.UTCTime)
modelCard_creationTime = Lens.lens (\ModelCard' {creationTime} -> creationTime) (\s@ModelCard' {} a -> s {creationTime = a} :: ModelCard) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
modelCard_lastModifiedBy :: Lens.Lens' ModelCard (Prelude.Maybe UserContext)
modelCard_lastModifiedBy = Lens.lens (\ModelCard' {lastModifiedBy} -> lastModifiedBy) (\s@ModelCard' {} a -> s {lastModifiedBy = a} :: ModelCard)

-- | The date and time that the model card was last modified.
modelCard_lastModifiedTime :: Lens.Lens' ModelCard (Prelude.Maybe Prelude.UTCTime)
modelCard_lastModifiedTime = Lens.lens (\ModelCard' {lastModifiedTime} -> lastModifiedTime) (\s@ModelCard' {} a -> s {lastModifiedTime = a} :: ModelCard) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the model card.
modelCard_modelCardArn :: Lens.Lens' ModelCard (Prelude.Maybe Prelude.Text)
modelCard_modelCardArn = Lens.lens (\ModelCard' {modelCardArn} -> modelCardArn) (\s@ModelCard' {} a -> s {modelCardArn = a} :: ModelCard)

-- | The unique name of the model card.
modelCard_modelCardName :: Lens.Lens' ModelCard (Prelude.Maybe Prelude.Text)
modelCard_modelCardName = Lens.lens (\ModelCard' {modelCardName} -> modelCardName) (\s@ModelCard' {} a -> s {modelCardName = a} :: ModelCard)

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
modelCard_modelCardStatus :: Lens.Lens' ModelCard (Prelude.Maybe ModelCardStatus)
modelCard_modelCardStatus = Lens.lens (\ModelCard' {modelCardStatus} -> modelCardStatus) (\s@ModelCard' {} a -> s {modelCardStatus = a} :: ModelCard)

-- | The version of the model card.
modelCard_modelCardVersion :: Lens.Lens' ModelCard (Prelude.Maybe Prelude.Int)
modelCard_modelCardVersion = Lens.lens (\ModelCard' {modelCardVersion} -> modelCardVersion) (\s@ModelCard' {} a -> s {modelCardVersion = a} :: ModelCard)

-- | The unique name (ID) of the model.
modelCard_modelId :: Lens.Lens' ModelCard (Prelude.Maybe Prelude.Text)
modelCard_modelId = Lens.lens (\ModelCard' {modelId} -> modelId) (\s@ModelCard' {} a -> s {modelId = a} :: ModelCard)

-- | The risk rating of the model. Different organizations might have
-- different criteria for model card risk ratings. For more information,
-- see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-cards-risk-rating.html Risk ratings>.
modelCard_riskRating :: Lens.Lens' ModelCard (Prelude.Maybe Prelude.Text)
modelCard_riskRating = Lens.lens (\ModelCard' {riskRating} -> riskRating) (\s@ModelCard' {} a -> s {riskRating = a} :: ModelCard)

-- | The security configuration used to protect model card data.
modelCard_securityConfig :: Lens.Lens' ModelCard (Prelude.Maybe ModelCardSecurityConfig)
modelCard_securityConfig = Lens.lens (\ModelCard' {securityConfig} -> securityConfig) (\s@ModelCard' {} a -> s {securityConfig = a} :: ModelCard)

-- | Key-value pairs used to manage metadata for the model card.
modelCard_tags :: Lens.Lens' ModelCard (Prelude.Maybe [Tag])
modelCard_tags = Lens.lens (\ModelCard' {tags} -> tags) (\s@ModelCard' {} a -> s {tags = a} :: ModelCard) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ModelCard where
  parseJSON =
    Data.withObject
      "ModelCard"
      ( \x ->
          ModelCard'
            Prelude.<$> (x Data..:? "Content")
            Prelude.<*> (x Data..:? "CreatedBy")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "LastModifiedBy")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "ModelCardArn")
            Prelude.<*> (x Data..:? "ModelCardName")
            Prelude.<*> (x Data..:? "ModelCardStatus")
            Prelude.<*> (x Data..:? "ModelCardVersion")
            Prelude.<*> (x Data..:? "ModelId")
            Prelude.<*> (x Data..:? "RiskRating")
            Prelude.<*> (x Data..:? "SecurityConfig")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ModelCard where
  hashWithSalt _salt ModelCard' {..} =
    _salt
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` createdBy
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedBy
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` modelCardArn
      `Prelude.hashWithSalt` modelCardName
      `Prelude.hashWithSalt` modelCardStatus
      `Prelude.hashWithSalt` modelCardVersion
      `Prelude.hashWithSalt` modelId
      `Prelude.hashWithSalt` riskRating
      `Prelude.hashWithSalt` securityConfig
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ModelCard where
  rnf ModelCard' {..} =
    Prelude.rnf content `Prelude.seq`
      Prelude.rnf createdBy `Prelude.seq`
        Prelude.rnf creationTime `Prelude.seq`
          Prelude.rnf lastModifiedBy `Prelude.seq`
            Prelude.rnf lastModifiedTime `Prelude.seq`
              Prelude.rnf modelCardArn `Prelude.seq`
                Prelude.rnf modelCardName `Prelude.seq`
                  Prelude.rnf modelCardStatus `Prelude.seq`
                    Prelude.rnf modelCardVersion `Prelude.seq`
                      Prelude.rnf modelId `Prelude.seq`
                        Prelude.rnf riskRating `Prelude.seq`
                          Prelude.rnf securityConfig `Prelude.seq`
                            Prelude.rnf tags
