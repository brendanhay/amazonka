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
-- Module      : Amazonka.SageMaker.Types.ModelDashboardModelCard
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ModelDashboardModelCard where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ModelCardSecurityConfig
import Amazonka.SageMaker.Types.ModelCardStatus
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.UserContext

-- | The model card for a model displayed in the Amazon SageMaker Model
-- Dashboard.
--
-- /See:/ 'newModelDashboardModelCard' smart constructor.
data ModelDashboardModelCard = ModelDashboardModelCard'
  { createdBy :: Prelude.Maybe UserContext,
    -- | A timestamp that indicates when the model card was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    lastModifiedBy :: Prelude.Maybe UserContext,
    -- | A timestamp that indicates when the model card was last updated.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) for a model card.
    modelCardArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a model card.
    modelCardName :: Prelude.Maybe Prelude.Text,
    -- | The model card status.
    modelCardStatus :: Prelude.Maybe ModelCardStatus,
    -- | The model card version.
    modelCardVersion :: Prelude.Maybe Prelude.Int,
    -- | For models created in SageMaker, this is the model ARN. For models
    -- created outside of SageMaker, this is a user-customized string.
    modelId :: Prelude.Maybe Prelude.Text,
    -- | A model card\'s risk rating. Can be low, medium, or high.
    riskRating :: Prelude.Maybe Prelude.Text,
    -- | The KMS Key ID (@KMSKeyId@) for encryption of model card information.
    securityConfig :: Prelude.Maybe ModelCardSecurityConfig,
    -- | The tags associated with a model card.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModelDashboardModelCard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdBy', 'modelDashboardModelCard_createdBy' - Undocumented member.
--
-- 'creationTime', 'modelDashboardModelCard_creationTime' - A timestamp that indicates when the model card was created.
--
-- 'lastModifiedBy', 'modelDashboardModelCard_lastModifiedBy' - Undocumented member.
--
-- 'lastModifiedTime', 'modelDashboardModelCard_lastModifiedTime' - A timestamp that indicates when the model card was last updated.
--
-- 'modelCardArn', 'modelDashboardModelCard_modelCardArn' - The Amazon Resource Name (ARN) for a model card.
--
-- 'modelCardName', 'modelDashboardModelCard_modelCardName' - The name of a model card.
--
-- 'modelCardStatus', 'modelDashboardModelCard_modelCardStatus' - The model card status.
--
-- 'modelCardVersion', 'modelDashboardModelCard_modelCardVersion' - The model card version.
--
-- 'modelId', 'modelDashboardModelCard_modelId' - For models created in SageMaker, this is the model ARN. For models
-- created outside of SageMaker, this is a user-customized string.
--
-- 'riskRating', 'modelDashboardModelCard_riskRating' - A model card\'s risk rating. Can be low, medium, or high.
--
-- 'securityConfig', 'modelDashboardModelCard_securityConfig' - The KMS Key ID (@KMSKeyId@) for encryption of model card information.
--
-- 'tags', 'modelDashboardModelCard_tags' - The tags associated with a model card.
newModelDashboardModelCard ::
  ModelDashboardModelCard
newModelDashboardModelCard =
  ModelDashboardModelCard'
    { createdBy =
        Prelude.Nothing,
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

-- | Undocumented member.
modelDashboardModelCard_createdBy :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe UserContext)
modelDashboardModelCard_createdBy = Lens.lens (\ModelDashboardModelCard' {createdBy} -> createdBy) (\s@ModelDashboardModelCard' {} a -> s {createdBy = a} :: ModelDashboardModelCard)

-- | A timestamp that indicates when the model card was created.
modelDashboardModelCard_creationTime :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe Prelude.UTCTime)
modelDashboardModelCard_creationTime = Lens.lens (\ModelDashboardModelCard' {creationTime} -> creationTime) (\s@ModelDashboardModelCard' {} a -> s {creationTime = a} :: ModelDashboardModelCard) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
modelDashboardModelCard_lastModifiedBy :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe UserContext)
modelDashboardModelCard_lastModifiedBy = Lens.lens (\ModelDashboardModelCard' {lastModifiedBy} -> lastModifiedBy) (\s@ModelDashboardModelCard' {} a -> s {lastModifiedBy = a} :: ModelDashboardModelCard)

-- | A timestamp that indicates when the model card was last updated.
modelDashboardModelCard_lastModifiedTime :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe Prelude.UTCTime)
modelDashboardModelCard_lastModifiedTime = Lens.lens (\ModelDashboardModelCard' {lastModifiedTime} -> lastModifiedTime) (\s@ModelDashboardModelCard' {} a -> s {lastModifiedTime = a} :: ModelDashboardModelCard) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) for a model card.
modelDashboardModelCard_modelCardArn :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe Prelude.Text)
modelDashboardModelCard_modelCardArn = Lens.lens (\ModelDashboardModelCard' {modelCardArn} -> modelCardArn) (\s@ModelDashboardModelCard' {} a -> s {modelCardArn = a} :: ModelDashboardModelCard)

-- | The name of a model card.
modelDashboardModelCard_modelCardName :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe Prelude.Text)
modelDashboardModelCard_modelCardName = Lens.lens (\ModelDashboardModelCard' {modelCardName} -> modelCardName) (\s@ModelDashboardModelCard' {} a -> s {modelCardName = a} :: ModelDashboardModelCard)

-- | The model card status.
modelDashboardModelCard_modelCardStatus :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe ModelCardStatus)
modelDashboardModelCard_modelCardStatus = Lens.lens (\ModelDashboardModelCard' {modelCardStatus} -> modelCardStatus) (\s@ModelDashboardModelCard' {} a -> s {modelCardStatus = a} :: ModelDashboardModelCard)

-- | The model card version.
modelDashboardModelCard_modelCardVersion :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe Prelude.Int)
modelDashboardModelCard_modelCardVersion = Lens.lens (\ModelDashboardModelCard' {modelCardVersion} -> modelCardVersion) (\s@ModelDashboardModelCard' {} a -> s {modelCardVersion = a} :: ModelDashboardModelCard)

-- | For models created in SageMaker, this is the model ARN. For models
-- created outside of SageMaker, this is a user-customized string.
modelDashboardModelCard_modelId :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe Prelude.Text)
modelDashboardModelCard_modelId = Lens.lens (\ModelDashboardModelCard' {modelId} -> modelId) (\s@ModelDashboardModelCard' {} a -> s {modelId = a} :: ModelDashboardModelCard)

-- | A model card\'s risk rating. Can be low, medium, or high.
modelDashboardModelCard_riskRating :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe Prelude.Text)
modelDashboardModelCard_riskRating = Lens.lens (\ModelDashboardModelCard' {riskRating} -> riskRating) (\s@ModelDashboardModelCard' {} a -> s {riskRating = a} :: ModelDashboardModelCard)

-- | The KMS Key ID (@KMSKeyId@) for encryption of model card information.
modelDashboardModelCard_securityConfig :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe ModelCardSecurityConfig)
modelDashboardModelCard_securityConfig = Lens.lens (\ModelDashboardModelCard' {securityConfig} -> securityConfig) (\s@ModelDashboardModelCard' {} a -> s {securityConfig = a} :: ModelDashboardModelCard)

-- | The tags associated with a model card.
modelDashboardModelCard_tags :: Lens.Lens' ModelDashboardModelCard (Prelude.Maybe [Tag])
modelDashboardModelCard_tags = Lens.lens (\ModelDashboardModelCard' {tags} -> tags) (\s@ModelDashboardModelCard' {} a -> s {tags = a} :: ModelDashboardModelCard) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ModelDashboardModelCard where
  parseJSON =
    Data.withObject
      "ModelDashboardModelCard"
      ( \x ->
          ModelDashboardModelCard'
            Prelude.<$> (x Data..:? "CreatedBy")
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

instance Prelude.Hashable ModelDashboardModelCard where
  hashWithSalt _salt ModelDashboardModelCard' {..} =
    _salt
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

instance Prelude.NFData ModelDashboardModelCard where
  rnf ModelDashboardModelCard' {..} =
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
