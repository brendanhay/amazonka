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
-- Module      : Amazonka.Kendra.Types.FaqSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FaqSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.FaqFileFormat
import Amazonka.Kendra.Types.FaqStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary information for frequently asked questions and answers included
-- in an index.
--
-- /See:/ 'newFaqSummary' smart constructor.
data FaqSummary = FaqSummary'
  { -- | The Unix timestamp when the FAQ was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The file type used to create the FAQ.
    fileFormat :: Prelude.Maybe FaqFileFormat,
    -- | The identifier of the FAQ.
    id :: Prelude.Maybe Prelude.Text,
    -- | The code for a language. This shows a supported language for the FAQ
    -- document as part of the summary information for FAQs. English is
    -- supported by default. For more information on supported languages,
    -- including their codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned the FAQ when you created or updated the FAQ.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the FAQ. When the status is @ACTIVE@ the FAQ is
    -- ready for use.
    status :: Prelude.Maybe FaqStatus,
    -- | The Unix timestamp when the FAQ was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FaqSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'faqSummary_createdAt' - The Unix timestamp when the FAQ was created.
--
-- 'fileFormat', 'faqSummary_fileFormat' - The file type used to create the FAQ.
--
-- 'id', 'faqSummary_id' - The identifier of the FAQ.
--
-- 'languageCode', 'faqSummary_languageCode' - The code for a language. This shows a supported language for the FAQ
-- document as part of the summary information for FAQs. English is
-- supported by default. For more information on supported languages,
-- including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'name', 'faqSummary_name' - The name that you assigned the FAQ when you created or updated the FAQ.
--
-- 'status', 'faqSummary_status' - The current status of the FAQ. When the status is @ACTIVE@ the FAQ is
-- ready for use.
--
-- 'updatedAt', 'faqSummary_updatedAt' - The Unix timestamp when the FAQ was last updated.
newFaqSummary ::
  FaqSummary
newFaqSummary =
  FaqSummary'
    { createdAt = Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      id = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Unix timestamp when the FAQ was created.
faqSummary_createdAt :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.UTCTime)
faqSummary_createdAt = Lens.lens (\FaqSummary' {createdAt} -> createdAt) (\s@FaqSummary' {} a -> s {createdAt = a} :: FaqSummary) Prelude.. Lens.mapping Data._Time

-- | The file type used to create the FAQ.
faqSummary_fileFormat :: Lens.Lens' FaqSummary (Prelude.Maybe FaqFileFormat)
faqSummary_fileFormat = Lens.lens (\FaqSummary' {fileFormat} -> fileFormat) (\s@FaqSummary' {} a -> s {fileFormat = a} :: FaqSummary)

-- | The identifier of the FAQ.
faqSummary_id :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.Text)
faqSummary_id = Lens.lens (\FaqSummary' {id} -> id) (\s@FaqSummary' {} a -> s {id = a} :: FaqSummary)

-- | The code for a language. This shows a supported language for the FAQ
-- document as part of the summary information for FAQs. English is
-- supported by default. For more information on supported languages,
-- including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
faqSummary_languageCode :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.Text)
faqSummary_languageCode = Lens.lens (\FaqSummary' {languageCode} -> languageCode) (\s@FaqSummary' {} a -> s {languageCode = a} :: FaqSummary)

-- | The name that you assigned the FAQ when you created or updated the FAQ.
faqSummary_name :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.Text)
faqSummary_name = Lens.lens (\FaqSummary' {name} -> name) (\s@FaqSummary' {} a -> s {name = a} :: FaqSummary)

-- | The current status of the FAQ. When the status is @ACTIVE@ the FAQ is
-- ready for use.
faqSummary_status :: Lens.Lens' FaqSummary (Prelude.Maybe FaqStatus)
faqSummary_status = Lens.lens (\FaqSummary' {status} -> status) (\s@FaqSummary' {} a -> s {status = a} :: FaqSummary)

-- | The Unix timestamp when the FAQ was last updated.
faqSummary_updatedAt :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.UTCTime)
faqSummary_updatedAt = Lens.lens (\FaqSummary' {updatedAt} -> updatedAt) (\s@FaqSummary' {} a -> s {updatedAt = a} :: FaqSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON FaqSummary where
  parseJSON =
    Data.withObject
      "FaqSummary"
      ( \x ->
          FaqSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "FileFormat")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable FaqSummary where
  hashWithSalt _salt FaqSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` fileFormat
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData FaqSummary where
  rnf FaqSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf fileFormat
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
