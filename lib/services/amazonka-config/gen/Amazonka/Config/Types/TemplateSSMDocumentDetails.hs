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
-- Module      : Amazonka.Config.Types.TemplateSSMDocumentDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.TemplateSSMDocumentDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This API allows you to create a conformance pack template with an Amazon
-- Web Services Systems Manager document (SSM document). To deploy a
-- conformance pack using an SSM document, first create an SSM document
-- with conformance pack content, and then provide the @DocumentName@ in
-- the
-- <https://docs.aws.amazon.com/config/latest/APIReference/API_PutConformancePack.html PutConformancePack API>.
-- You can also provide the @DocumentVersion@.
--
-- The @TemplateSSMDocumentDetails@ object contains the name of the SSM
-- document and the version of the SSM document.
--
-- /See:/ 'newTemplateSSMDocumentDetails' smart constructor.
data TemplateSSMDocumentDetails = TemplateSSMDocumentDetails'
  { -- | The version of the SSM document to use to create a conformance pack. By
    -- default, Config uses the latest version.
    --
    -- This field is optional.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the SSM document to use to
    -- create a conformance pack. If you use the document name, Config checks
    -- only your account and Amazon Web Services Region for the SSM document.
    -- If you want to use an SSM document from another Region or account, you
    -- must provide the ARN.
    documentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateSSMDocumentDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentVersion', 'templateSSMDocumentDetails_documentVersion' - The version of the SSM document to use to create a conformance pack. By
-- default, Config uses the latest version.
--
-- This field is optional.
--
-- 'documentName', 'templateSSMDocumentDetails_documentName' - The name or Amazon Resource Name (ARN) of the SSM document to use to
-- create a conformance pack. If you use the document name, Config checks
-- only your account and Amazon Web Services Region for the SSM document.
-- If you want to use an SSM document from another Region or account, you
-- must provide the ARN.
newTemplateSSMDocumentDetails ::
  -- | 'documentName'
  Prelude.Text ->
  TemplateSSMDocumentDetails
newTemplateSSMDocumentDetails pDocumentName_ =
  TemplateSSMDocumentDetails'
    { documentVersion =
        Prelude.Nothing,
      documentName = pDocumentName_
    }

-- | The version of the SSM document to use to create a conformance pack. By
-- default, Config uses the latest version.
--
-- This field is optional.
templateSSMDocumentDetails_documentVersion :: Lens.Lens' TemplateSSMDocumentDetails (Prelude.Maybe Prelude.Text)
templateSSMDocumentDetails_documentVersion = Lens.lens (\TemplateSSMDocumentDetails' {documentVersion} -> documentVersion) (\s@TemplateSSMDocumentDetails' {} a -> s {documentVersion = a} :: TemplateSSMDocumentDetails)

-- | The name or Amazon Resource Name (ARN) of the SSM document to use to
-- create a conformance pack. If you use the document name, Config checks
-- only your account and Amazon Web Services Region for the SSM document.
-- If you want to use an SSM document from another Region or account, you
-- must provide the ARN.
templateSSMDocumentDetails_documentName :: Lens.Lens' TemplateSSMDocumentDetails Prelude.Text
templateSSMDocumentDetails_documentName = Lens.lens (\TemplateSSMDocumentDetails' {documentName} -> documentName) (\s@TemplateSSMDocumentDetails' {} a -> s {documentName = a} :: TemplateSSMDocumentDetails)

instance Data.FromJSON TemplateSSMDocumentDetails where
  parseJSON =
    Data.withObject
      "TemplateSSMDocumentDetails"
      ( \x ->
          TemplateSSMDocumentDetails'
            Prelude.<$> (x Data..:? "DocumentVersion")
            Prelude.<*> (x Data..: "DocumentName")
      )

instance Prelude.Hashable TemplateSSMDocumentDetails where
  hashWithSalt _salt TemplateSSMDocumentDetails' {..} =
    _salt
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` documentName

instance Prelude.NFData TemplateSSMDocumentDetails where
  rnf TemplateSSMDocumentDetails' {..} =
    Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf documentName

instance Data.ToJSON TemplateSSMDocumentDetails where
  toJSON TemplateSSMDocumentDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            Prelude.Just ("DocumentName" Data..= documentName)
          ]
      )
