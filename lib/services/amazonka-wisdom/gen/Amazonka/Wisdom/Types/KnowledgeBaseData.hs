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
-- Module      : Amazonka.Wisdom.Types.KnowledgeBaseData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.KnowledgeBaseData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.KnowledgeBaseStatus
import Amazonka.Wisdom.Types.KnowledgeBaseType
import Amazonka.Wisdom.Types.RenderingConfiguration
import Amazonka.Wisdom.Types.ServerSideEncryptionConfiguration
import Amazonka.Wisdom.Types.SourceConfiguration

-- | Information about the knowledge base.
--
-- /See:/ 'newKnowledgeBaseData' smart constructor.
data KnowledgeBaseData = KnowledgeBaseData'
  { -- | Information about how to render the content.
    renderingConfiguration :: Prelude.Maybe RenderingConfiguration,
    -- | Source configuration information about the knowledge base.
    sourceConfiguration :: Prelude.Maybe SourceConfiguration,
    -- | An epoch timestamp indicating the most recent content modification
    -- inside the knowledge base. If no content exists in a knowledge base,
    -- this value is unset.
    lastContentModificationTime :: Prelude.Maybe Core.POSIX,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The KMS key used for encryption.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the knowledge base.
    knowledgeBaseArn :: Prelude.Text,
    -- | The the identifier of the knowledge base.
    knowledgeBaseId :: Prelude.Text,
    -- | The type of knowledge base.
    knowledgeBaseType :: KnowledgeBaseType,
    -- | The name of the knowledge base.
    name :: Prelude.Text,
    -- | The status of the knowledge base.
    status :: KnowledgeBaseStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KnowledgeBaseData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'renderingConfiguration', 'knowledgeBaseData_renderingConfiguration' - Information about how to render the content.
--
-- 'sourceConfiguration', 'knowledgeBaseData_sourceConfiguration' - Source configuration information about the knowledge base.
--
-- 'lastContentModificationTime', 'knowledgeBaseData_lastContentModificationTime' - An epoch timestamp indicating the most recent content modification
-- inside the knowledge base. If no content exists in a knowledge base,
-- this value is unset.
--
-- 'description', 'knowledgeBaseData_description' - The description.
--
-- 'serverSideEncryptionConfiguration', 'knowledgeBaseData_serverSideEncryptionConfiguration' - The KMS key used for encryption.
--
-- 'tags', 'knowledgeBaseData_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'knowledgeBaseArn', 'knowledgeBaseData_knowledgeBaseArn' - The Amazon Resource Name (ARN) of the knowledge base.
--
-- 'knowledgeBaseId', 'knowledgeBaseData_knowledgeBaseId' - The the identifier of the knowledge base.
--
-- 'knowledgeBaseType', 'knowledgeBaseData_knowledgeBaseType' - The type of knowledge base.
--
-- 'name', 'knowledgeBaseData_name' - The name of the knowledge base.
--
-- 'status', 'knowledgeBaseData_status' - The status of the knowledge base.
newKnowledgeBaseData ::
  -- | 'knowledgeBaseArn'
  Prelude.Text ->
  -- | 'knowledgeBaseId'
  Prelude.Text ->
  -- | 'knowledgeBaseType'
  KnowledgeBaseType ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  KnowledgeBaseStatus ->
  KnowledgeBaseData
newKnowledgeBaseData
  pKnowledgeBaseArn_
  pKnowledgeBaseId_
  pKnowledgeBaseType_
  pName_
  pStatus_ =
    KnowledgeBaseData'
      { renderingConfiguration =
          Prelude.Nothing,
        sourceConfiguration = Prelude.Nothing,
        lastContentModificationTime = Prelude.Nothing,
        description = Prelude.Nothing,
        serverSideEncryptionConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        knowledgeBaseArn = pKnowledgeBaseArn_,
        knowledgeBaseId = pKnowledgeBaseId_,
        knowledgeBaseType = pKnowledgeBaseType_,
        name = pName_,
        status = pStatus_
      }

-- | Information about how to render the content.
knowledgeBaseData_renderingConfiguration :: Lens.Lens' KnowledgeBaseData (Prelude.Maybe RenderingConfiguration)
knowledgeBaseData_renderingConfiguration = Lens.lens (\KnowledgeBaseData' {renderingConfiguration} -> renderingConfiguration) (\s@KnowledgeBaseData' {} a -> s {renderingConfiguration = a} :: KnowledgeBaseData)

-- | Source configuration information about the knowledge base.
knowledgeBaseData_sourceConfiguration :: Lens.Lens' KnowledgeBaseData (Prelude.Maybe SourceConfiguration)
knowledgeBaseData_sourceConfiguration = Lens.lens (\KnowledgeBaseData' {sourceConfiguration} -> sourceConfiguration) (\s@KnowledgeBaseData' {} a -> s {sourceConfiguration = a} :: KnowledgeBaseData)

-- | An epoch timestamp indicating the most recent content modification
-- inside the knowledge base. If no content exists in a knowledge base,
-- this value is unset.
knowledgeBaseData_lastContentModificationTime :: Lens.Lens' KnowledgeBaseData (Prelude.Maybe Prelude.UTCTime)
knowledgeBaseData_lastContentModificationTime = Lens.lens (\KnowledgeBaseData' {lastContentModificationTime} -> lastContentModificationTime) (\s@KnowledgeBaseData' {} a -> s {lastContentModificationTime = a} :: KnowledgeBaseData) Prelude.. Lens.mapping Core._Time

-- | The description.
knowledgeBaseData_description :: Lens.Lens' KnowledgeBaseData (Prelude.Maybe Prelude.Text)
knowledgeBaseData_description = Lens.lens (\KnowledgeBaseData' {description} -> description) (\s@KnowledgeBaseData' {} a -> s {description = a} :: KnowledgeBaseData)

-- | The KMS key used for encryption.
knowledgeBaseData_serverSideEncryptionConfiguration :: Lens.Lens' KnowledgeBaseData (Prelude.Maybe ServerSideEncryptionConfiguration)
knowledgeBaseData_serverSideEncryptionConfiguration = Lens.lens (\KnowledgeBaseData' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@KnowledgeBaseData' {} a -> s {serverSideEncryptionConfiguration = a} :: KnowledgeBaseData)

-- | The tags used to organize, track, or control access for this resource.
knowledgeBaseData_tags :: Lens.Lens' KnowledgeBaseData (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
knowledgeBaseData_tags = Lens.lens (\KnowledgeBaseData' {tags} -> tags) (\s@KnowledgeBaseData' {} a -> s {tags = a} :: KnowledgeBaseData) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the knowledge base.
knowledgeBaseData_knowledgeBaseArn :: Lens.Lens' KnowledgeBaseData Prelude.Text
knowledgeBaseData_knowledgeBaseArn = Lens.lens (\KnowledgeBaseData' {knowledgeBaseArn} -> knowledgeBaseArn) (\s@KnowledgeBaseData' {} a -> s {knowledgeBaseArn = a} :: KnowledgeBaseData)

-- | The the identifier of the knowledge base.
knowledgeBaseData_knowledgeBaseId :: Lens.Lens' KnowledgeBaseData Prelude.Text
knowledgeBaseData_knowledgeBaseId = Lens.lens (\KnowledgeBaseData' {knowledgeBaseId} -> knowledgeBaseId) (\s@KnowledgeBaseData' {} a -> s {knowledgeBaseId = a} :: KnowledgeBaseData)

-- | The type of knowledge base.
knowledgeBaseData_knowledgeBaseType :: Lens.Lens' KnowledgeBaseData KnowledgeBaseType
knowledgeBaseData_knowledgeBaseType = Lens.lens (\KnowledgeBaseData' {knowledgeBaseType} -> knowledgeBaseType) (\s@KnowledgeBaseData' {} a -> s {knowledgeBaseType = a} :: KnowledgeBaseData)

-- | The name of the knowledge base.
knowledgeBaseData_name :: Lens.Lens' KnowledgeBaseData Prelude.Text
knowledgeBaseData_name = Lens.lens (\KnowledgeBaseData' {name} -> name) (\s@KnowledgeBaseData' {} a -> s {name = a} :: KnowledgeBaseData)

-- | The status of the knowledge base.
knowledgeBaseData_status :: Lens.Lens' KnowledgeBaseData KnowledgeBaseStatus
knowledgeBaseData_status = Lens.lens (\KnowledgeBaseData' {status} -> status) (\s@KnowledgeBaseData' {} a -> s {status = a} :: KnowledgeBaseData)

instance Core.FromJSON KnowledgeBaseData where
  parseJSON =
    Core.withObject
      "KnowledgeBaseData"
      ( \x ->
          KnowledgeBaseData'
            Prelude.<$> (x Core..:? "renderingConfiguration")
            Prelude.<*> (x Core..:? "sourceConfiguration")
            Prelude.<*> (x Core..:? "lastContentModificationTime")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "serverSideEncryptionConfiguration")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "knowledgeBaseArn")
            Prelude.<*> (x Core..: "knowledgeBaseId")
            Prelude.<*> (x Core..: "knowledgeBaseType")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable KnowledgeBaseData where
  hashWithSalt _salt KnowledgeBaseData' {..} =
    _salt `Prelude.hashWithSalt` renderingConfiguration
      `Prelude.hashWithSalt` sourceConfiguration
      `Prelude.hashWithSalt` lastContentModificationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` knowledgeBaseArn
      `Prelude.hashWithSalt` knowledgeBaseId
      `Prelude.hashWithSalt` knowledgeBaseType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData KnowledgeBaseData where
  rnf KnowledgeBaseData' {..} =
    Prelude.rnf renderingConfiguration
      `Prelude.seq` Prelude.rnf sourceConfiguration
      `Prelude.seq` Prelude.rnf lastContentModificationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf knowledgeBaseArn
      `Prelude.seq` Prelude.rnf knowledgeBaseId
      `Prelude.seq` Prelude.rnf knowledgeBaseType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
