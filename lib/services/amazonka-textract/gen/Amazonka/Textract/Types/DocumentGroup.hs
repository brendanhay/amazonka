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
-- Module      : Amazonka.Textract.Types.DocumentGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.DocumentGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.DetectedSignature
import Amazonka.Textract.Types.SplitDocument
import Amazonka.Textract.Types.UndetectedSignature

-- | Summary information about documents grouped by the same document type.
--
-- /See:/ 'newDocumentGroup' smart constructor.
data DocumentGroup = DocumentGroup'
  { -- | A list of the detected signatures found in a document group.
    detectedSignatures :: Prelude.Maybe [DetectedSignature],
    -- | An array that contains information about the pages of a document,
    -- defined by logical boundary.
    splitDocuments :: Prelude.Maybe [SplitDocument],
    -- | The type of document that Amazon Textract has detected. See LINK for a
    -- list of all types returned by Textract.
    type' :: Prelude.Maybe Prelude.Text,
    -- | A list of any expected signatures not found in a document group.
    undetectedSignatures :: Prelude.Maybe [UndetectedSignature]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectedSignatures', 'documentGroup_detectedSignatures' - A list of the detected signatures found in a document group.
--
-- 'splitDocuments', 'documentGroup_splitDocuments' - An array that contains information about the pages of a document,
-- defined by logical boundary.
--
-- 'type'', 'documentGroup_type' - The type of document that Amazon Textract has detected. See LINK for a
-- list of all types returned by Textract.
--
-- 'undetectedSignatures', 'documentGroup_undetectedSignatures' - A list of any expected signatures not found in a document group.
newDocumentGroup ::
  DocumentGroup
newDocumentGroup =
  DocumentGroup'
    { detectedSignatures =
        Prelude.Nothing,
      splitDocuments = Prelude.Nothing,
      type' = Prelude.Nothing,
      undetectedSignatures = Prelude.Nothing
    }

-- | A list of the detected signatures found in a document group.
documentGroup_detectedSignatures :: Lens.Lens' DocumentGroup (Prelude.Maybe [DetectedSignature])
documentGroup_detectedSignatures = Lens.lens (\DocumentGroup' {detectedSignatures} -> detectedSignatures) (\s@DocumentGroup' {} a -> s {detectedSignatures = a} :: DocumentGroup) Prelude.. Lens.mapping Lens.coerced

-- | An array that contains information about the pages of a document,
-- defined by logical boundary.
documentGroup_splitDocuments :: Lens.Lens' DocumentGroup (Prelude.Maybe [SplitDocument])
documentGroup_splitDocuments = Lens.lens (\DocumentGroup' {splitDocuments} -> splitDocuments) (\s@DocumentGroup' {} a -> s {splitDocuments = a} :: DocumentGroup) Prelude.. Lens.mapping Lens.coerced

-- | The type of document that Amazon Textract has detected. See LINK for a
-- list of all types returned by Textract.
documentGroup_type :: Lens.Lens' DocumentGroup (Prelude.Maybe Prelude.Text)
documentGroup_type = Lens.lens (\DocumentGroup' {type'} -> type') (\s@DocumentGroup' {} a -> s {type' = a} :: DocumentGroup)

-- | A list of any expected signatures not found in a document group.
documentGroup_undetectedSignatures :: Lens.Lens' DocumentGroup (Prelude.Maybe [UndetectedSignature])
documentGroup_undetectedSignatures = Lens.lens (\DocumentGroup' {undetectedSignatures} -> undetectedSignatures) (\s@DocumentGroup' {} a -> s {undetectedSignatures = a} :: DocumentGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DocumentGroup where
  parseJSON =
    Data.withObject
      "DocumentGroup"
      ( \x ->
          DocumentGroup'
            Prelude.<$> ( x Data..:? "DetectedSignatures"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SplitDocuments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> ( x Data..:? "UndetectedSignatures"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DocumentGroup where
  hashWithSalt _salt DocumentGroup' {..} =
    _salt `Prelude.hashWithSalt` detectedSignatures
      `Prelude.hashWithSalt` splitDocuments
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` undetectedSignatures

instance Prelude.NFData DocumentGroup where
  rnf DocumentGroup' {..} =
    Prelude.rnf detectedSignatures
      `Prelude.seq` Prelude.rnf splitDocuments
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf undetectedSignatures
