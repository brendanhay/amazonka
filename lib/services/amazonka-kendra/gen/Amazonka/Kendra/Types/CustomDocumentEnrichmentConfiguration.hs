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
-- Module      : Amazonka.Kendra.Types.CustomDocumentEnrichmentConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.CustomDocumentEnrichmentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.HookConfiguration
import Amazonka.Kendra.Types.InlineCustomDocumentEnrichmentConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides the configuration information for altering document metadata
-- and content during the document ingestion process.
--
-- For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html Customizing document metadata during the ingestion process>.
--
-- /See:/ 'newCustomDocumentEnrichmentConfiguration' smart constructor.
data CustomDocumentEnrichmentConfiguration = CustomDocumentEnrichmentConfiguration'
  { -- | The Amazon Resource Name (ARN) of a role with permission to run
    -- @PreExtractionHookConfiguration@ and @PostExtractionHookConfiguration@
    -- for altering document metadata and content during the document ingestion
    -- process. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM roles for Amazon Kendra>.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Configuration information to alter document attributes or metadata
    -- fields and content when ingesting documents into Amazon Kendra.
    inlineConfigurations :: Prelude.Maybe [InlineCustomDocumentEnrichmentConfiguration],
    -- | Configuration information for invoking a Lambda function in Lambda on
    -- the structured documents with their metadata and text extracted. You can
    -- use a Lambda function to apply advanced logic for creating, modifying,
    -- or deleting document metadata and content. For more information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html#advanced-data-manipulation Advanced data manipulation>.
    postExtractionHookConfiguration :: Prelude.Maybe HookConfiguration,
    -- | Configuration information for invoking a Lambda function in Lambda on
    -- the original or raw documents before extracting their metadata and text.
    -- You can use a Lambda function to apply advanced logic for creating,
    -- modifying, or deleting document metadata and content. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html#advanced-data-manipulation Advanced data manipulation>.
    preExtractionHookConfiguration :: Prelude.Maybe HookConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomDocumentEnrichmentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'customDocumentEnrichmentConfiguration_roleArn' - The Amazon Resource Name (ARN) of a role with permission to run
-- @PreExtractionHookConfiguration@ and @PostExtractionHookConfiguration@
-- for altering document metadata and content during the document ingestion
-- process. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM roles for Amazon Kendra>.
--
-- 'inlineConfigurations', 'customDocumentEnrichmentConfiguration_inlineConfigurations' - Configuration information to alter document attributes or metadata
-- fields and content when ingesting documents into Amazon Kendra.
--
-- 'postExtractionHookConfiguration', 'customDocumentEnrichmentConfiguration_postExtractionHookConfiguration' - Configuration information for invoking a Lambda function in Lambda on
-- the structured documents with their metadata and text extracted. You can
-- use a Lambda function to apply advanced logic for creating, modifying,
-- or deleting document metadata and content. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html#advanced-data-manipulation Advanced data manipulation>.
--
-- 'preExtractionHookConfiguration', 'customDocumentEnrichmentConfiguration_preExtractionHookConfiguration' - Configuration information for invoking a Lambda function in Lambda on
-- the original or raw documents before extracting their metadata and text.
-- You can use a Lambda function to apply advanced logic for creating,
-- modifying, or deleting document metadata and content. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html#advanced-data-manipulation Advanced data manipulation>.
newCustomDocumentEnrichmentConfiguration ::
  CustomDocumentEnrichmentConfiguration
newCustomDocumentEnrichmentConfiguration =
  CustomDocumentEnrichmentConfiguration'
    { roleArn =
        Prelude.Nothing,
      inlineConfigurations =
        Prelude.Nothing,
      postExtractionHookConfiguration =
        Prelude.Nothing,
      preExtractionHookConfiguration =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of a role with permission to run
-- @PreExtractionHookConfiguration@ and @PostExtractionHookConfiguration@
-- for altering document metadata and content during the document ingestion
-- process. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/iam-roles.html IAM roles for Amazon Kendra>.
customDocumentEnrichmentConfiguration_roleArn :: Lens.Lens' CustomDocumentEnrichmentConfiguration (Prelude.Maybe Prelude.Text)
customDocumentEnrichmentConfiguration_roleArn = Lens.lens (\CustomDocumentEnrichmentConfiguration' {roleArn} -> roleArn) (\s@CustomDocumentEnrichmentConfiguration' {} a -> s {roleArn = a} :: CustomDocumentEnrichmentConfiguration)

-- | Configuration information to alter document attributes or metadata
-- fields and content when ingesting documents into Amazon Kendra.
customDocumentEnrichmentConfiguration_inlineConfigurations :: Lens.Lens' CustomDocumentEnrichmentConfiguration (Prelude.Maybe [InlineCustomDocumentEnrichmentConfiguration])
customDocumentEnrichmentConfiguration_inlineConfigurations = Lens.lens (\CustomDocumentEnrichmentConfiguration' {inlineConfigurations} -> inlineConfigurations) (\s@CustomDocumentEnrichmentConfiguration' {} a -> s {inlineConfigurations = a} :: CustomDocumentEnrichmentConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information for invoking a Lambda function in Lambda on
-- the structured documents with their metadata and text extracted. You can
-- use a Lambda function to apply advanced logic for creating, modifying,
-- or deleting document metadata and content. For more information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html#advanced-data-manipulation Advanced data manipulation>.
customDocumentEnrichmentConfiguration_postExtractionHookConfiguration :: Lens.Lens' CustomDocumentEnrichmentConfiguration (Prelude.Maybe HookConfiguration)
customDocumentEnrichmentConfiguration_postExtractionHookConfiguration = Lens.lens (\CustomDocumentEnrichmentConfiguration' {postExtractionHookConfiguration} -> postExtractionHookConfiguration) (\s@CustomDocumentEnrichmentConfiguration' {} a -> s {postExtractionHookConfiguration = a} :: CustomDocumentEnrichmentConfiguration)

-- | Configuration information for invoking a Lambda function in Lambda on
-- the original or raw documents before extracting their metadata and text.
-- You can use a Lambda function to apply advanced logic for creating,
-- modifying, or deleting document metadata and content. For more
-- information, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/custom-document-enrichment.html#advanced-data-manipulation Advanced data manipulation>.
customDocumentEnrichmentConfiguration_preExtractionHookConfiguration :: Lens.Lens' CustomDocumentEnrichmentConfiguration (Prelude.Maybe HookConfiguration)
customDocumentEnrichmentConfiguration_preExtractionHookConfiguration = Lens.lens (\CustomDocumentEnrichmentConfiguration' {preExtractionHookConfiguration} -> preExtractionHookConfiguration) (\s@CustomDocumentEnrichmentConfiguration' {} a -> s {preExtractionHookConfiguration = a} :: CustomDocumentEnrichmentConfiguration)

instance
  Core.FromJSON
    CustomDocumentEnrichmentConfiguration
  where
  parseJSON =
    Core.withObject
      "CustomDocumentEnrichmentConfiguration"
      ( \x ->
          CustomDocumentEnrichmentConfiguration'
            Prelude.<$> (x Core..:? "RoleArn")
            Prelude.<*> ( x Core..:? "InlineConfigurations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PostExtractionHookConfiguration")
            Prelude.<*> (x Core..:? "PreExtractionHookConfiguration")
      )

instance
  Prelude.Hashable
    CustomDocumentEnrichmentConfiguration
  where
  hashWithSalt
    _salt
    CustomDocumentEnrichmentConfiguration' {..} =
      _salt `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` inlineConfigurations
        `Prelude.hashWithSalt` postExtractionHookConfiguration
        `Prelude.hashWithSalt` preExtractionHookConfiguration

instance
  Prelude.NFData
    CustomDocumentEnrichmentConfiguration
  where
  rnf CustomDocumentEnrichmentConfiguration' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf inlineConfigurations
      `Prelude.seq` Prelude.rnf postExtractionHookConfiguration
      `Prelude.seq` Prelude.rnf preExtractionHookConfiguration

instance
  Core.ToJSON
    CustomDocumentEnrichmentConfiguration
  where
  toJSON CustomDocumentEnrichmentConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RoleArn" Core..=) Prelude.<$> roleArn,
            ("InlineConfigurations" Core..=)
              Prelude.<$> inlineConfigurations,
            ("PostExtractionHookConfiguration" Core..=)
              Prelude.<$> postExtractionHookConfiguration,
            ("PreExtractionHookConfiguration" Core..=)
              Prelude.<$> preExtractionHookConfiguration
          ]
      )
