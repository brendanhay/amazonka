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
-- Module      : Amazonka.Wisdom.Types.KnowledgeBaseSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.KnowledgeBaseSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.KnowledgeBaseStatus
import Amazonka.Wisdom.Types.KnowledgeBaseType
import Amazonka.Wisdom.Types.RenderingConfiguration
import Amazonka.Wisdom.Types.ServerSideEncryptionConfiguration
import Amazonka.Wisdom.Types.SourceConfiguration

-- | Summary information about the knowledge base.
--
-- /See:/ 'newKnowledgeBaseSummary' smart constructor.
data KnowledgeBaseSummary = KnowledgeBaseSummary'
  { -- | The description of the knowledge base.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about how to render the content.
    renderingConfiguration :: Prelude.Maybe RenderingConfiguration,
    -- | The KMS key used for encryption.
    serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | Configuration information about the external data source.
    sourceConfiguration :: Prelude.Maybe SourceConfiguration,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the knowledge base.
    knowledgeBaseArn :: Prelude.Text,
    -- | The identifier of the knowledge base.
    knowledgeBaseId :: Prelude.Text,
    -- | The type of knowledge base.
    knowledgeBaseType :: KnowledgeBaseType,
    -- | The name of the knowledge base.
    name :: Prelude.Text,
    -- | The status of the knowledge base summary.
    status :: KnowledgeBaseStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KnowledgeBaseSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'knowledgeBaseSummary_description' - The description of the knowledge base.
--
-- 'renderingConfiguration', 'knowledgeBaseSummary_renderingConfiguration' - Information about how to render the content.
--
-- 'serverSideEncryptionConfiguration', 'knowledgeBaseSummary_serverSideEncryptionConfiguration' - The KMS key used for encryption.
--
-- 'sourceConfiguration', 'knowledgeBaseSummary_sourceConfiguration' - Configuration information about the external data source.
--
-- 'tags', 'knowledgeBaseSummary_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'knowledgeBaseArn', 'knowledgeBaseSummary_knowledgeBaseArn' - The Amazon Resource Name (ARN) of the knowledge base.
--
-- 'knowledgeBaseId', 'knowledgeBaseSummary_knowledgeBaseId' - The identifier of the knowledge base.
--
-- 'knowledgeBaseType', 'knowledgeBaseSummary_knowledgeBaseType' - The type of knowledge base.
--
-- 'name', 'knowledgeBaseSummary_name' - The name of the knowledge base.
--
-- 'status', 'knowledgeBaseSummary_status' - The status of the knowledge base summary.
newKnowledgeBaseSummary ::
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
  KnowledgeBaseSummary
newKnowledgeBaseSummary
  pKnowledgeBaseArn_
  pKnowledgeBaseId_
  pKnowledgeBaseType_
  pName_
  pStatus_ =
    KnowledgeBaseSummary'
      { description =
          Prelude.Nothing,
        renderingConfiguration = Prelude.Nothing,
        serverSideEncryptionConfiguration = Prelude.Nothing,
        sourceConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        knowledgeBaseArn = pKnowledgeBaseArn_,
        knowledgeBaseId = pKnowledgeBaseId_,
        knowledgeBaseType = pKnowledgeBaseType_,
        name = pName_,
        status = pStatus_
      }

-- | The description of the knowledge base.
knowledgeBaseSummary_description :: Lens.Lens' KnowledgeBaseSummary (Prelude.Maybe Prelude.Text)
knowledgeBaseSummary_description = Lens.lens (\KnowledgeBaseSummary' {description} -> description) (\s@KnowledgeBaseSummary' {} a -> s {description = a} :: KnowledgeBaseSummary)

-- | Information about how to render the content.
knowledgeBaseSummary_renderingConfiguration :: Lens.Lens' KnowledgeBaseSummary (Prelude.Maybe RenderingConfiguration)
knowledgeBaseSummary_renderingConfiguration = Lens.lens (\KnowledgeBaseSummary' {renderingConfiguration} -> renderingConfiguration) (\s@KnowledgeBaseSummary' {} a -> s {renderingConfiguration = a} :: KnowledgeBaseSummary)

-- | The KMS key used for encryption.
knowledgeBaseSummary_serverSideEncryptionConfiguration :: Lens.Lens' KnowledgeBaseSummary (Prelude.Maybe ServerSideEncryptionConfiguration)
knowledgeBaseSummary_serverSideEncryptionConfiguration = Lens.lens (\KnowledgeBaseSummary' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@KnowledgeBaseSummary' {} a -> s {serverSideEncryptionConfiguration = a} :: KnowledgeBaseSummary)

-- | Configuration information about the external data source.
knowledgeBaseSummary_sourceConfiguration :: Lens.Lens' KnowledgeBaseSummary (Prelude.Maybe SourceConfiguration)
knowledgeBaseSummary_sourceConfiguration = Lens.lens (\KnowledgeBaseSummary' {sourceConfiguration} -> sourceConfiguration) (\s@KnowledgeBaseSummary' {} a -> s {sourceConfiguration = a} :: KnowledgeBaseSummary)

-- | The tags used to organize, track, or control access for this resource.
knowledgeBaseSummary_tags :: Lens.Lens' KnowledgeBaseSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
knowledgeBaseSummary_tags = Lens.lens (\KnowledgeBaseSummary' {tags} -> tags) (\s@KnowledgeBaseSummary' {} a -> s {tags = a} :: KnowledgeBaseSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the knowledge base.
knowledgeBaseSummary_knowledgeBaseArn :: Lens.Lens' KnowledgeBaseSummary Prelude.Text
knowledgeBaseSummary_knowledgeBaseArn = Lens.lens (\KnowledgeBaseSummary' {knowledgeBaseArn} -> knowledgeBaseArn) (\s@KnowledgeBaseSummary' {} a -> s {knowledgeBaseArn = a} :: KnowledgeBaseSummary)

-- | The identifier of the knowledge base.
knowledgeBaseSummary_knowledgeBaseId :: Lens.Lens' KnowledgeBaseSummary Prelude.Text
knowledgeBaseSummary_knowledgeBaseId = Lens.lens (\KnowledgeBaseSummary' {knowledgeBaseId} -> knowledgeBaseId) (\s@KnowledgeBaseSummary' {} a -> s {knowledgeBaseId = a} :: KnowledgeBaseSummary)

-- | The type of knowledge base.
knowledgeBaseSummary_knowledgeBaseType :: Lens.Lens' KnowledgeBaseSummary KnowledgeBaseType
knowledgeBaseSummary_knowledgeBaseType = Lens.lens (\KnowledgeBaseSummary' {knowledgeBaseType} -> knowledgeBaseType) (\s@KnowledgeBaseSummary' {} a -> s {knowledgeBaseType = a} :: KnowledgeBaseSummary)

-- | The name of the knowledge base.
knowledgeBaseSummary_name :: Lens.Lens' KnowledgeBaseSummary Prelude.Text
knowledgeBaseSummary_name = Lens.lens (\KnowledgeBaseSummary' {name} -> name) (\s@KnowledgeBaseSummary' {} a -> s {name = a} :: KnowledgeBaseSummary)

-- | The status of the knowledge base summary.
knowledgeBaseSummary_status :: Lens.Lens' KnowledgeBaseSummary KnowledgeBaseStatus
knowledgeBaseSummary_status = Lens.lens (\KnowledgeBaseSummary' {status} -> status) (\s@KnowledgeBaseSummary' {} a -> s {status = a} :: KnowledgeBaseSummary)

instance Data.FromJSON KnowledgeBaseSummary where
  parseJSON =
    Data.withObject
      "KnowledgeBaseSummary"
      ( \x ->
          KnowledgeBaseSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "renderingConfiguration")
            Prelude.<*> (x Data..:? "serverSideEncryptionConfiguration")
            Prelude.<*> (x Data..:? "sourceConfiguration")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "knowledgeBaseArn")
            Prelude.<*> (x Data..: "knowledgeBaseId")
            Prelude.<*> (x Data..: "knowledgeBaseType")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable KnowledgeBaseSummary where
  hashWithSalt _salt KnowledgeBaseSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` renderingConfiguration
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration
      `Prelude.hashWithSalt` sourceConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` knowledgeBaseArn
      `Prelude.hashWithSalt` knowledgeBaseId
      `Prelude.hashWithSalt` knowledgeBaseType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData KnowledgeBaseSummary where
  rnf KnowledgeBaseSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf renderingConfiguration
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf sourceConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf knowledgeBaseArn
      `Prelude.seq` Prelude.rnf knowledgeBaseId
      `Prelude.seq` Prelude.rnf knowledgeBaseType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
