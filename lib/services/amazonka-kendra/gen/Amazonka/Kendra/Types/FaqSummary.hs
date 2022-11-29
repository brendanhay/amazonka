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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.FaqSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.FaqFileFormat
import Amazonka.Kendra.Types.FaqStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary information for frequently asked questions and answers included
-- in an index.
--
-- /See:/ 'newFaqSummary' smart constructor.
data FaqSummary = FaqSummary'
  { -- | The name that you assigned the FAQ when you created or updated the FAQ.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current status of the FAQ. When the status is @ACTIVE@ the FAQ is
    -- ready for use.
    status :: Prelude.Maybe FaqStatus,
    -- | The unique identifier of the FAQ.
    id :: Prelude.Maybe Prelude.Text,
    -- | The code for a language. This shows a supported language for the FAQ
    -- document as part of the summary information for FAQs. English is
    -- supported by default. For more information on supported languages,
    -- including their codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The UNIX datetime that the FAQ was added to the index.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The UNIX datetime that the FAQ was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The file type used to create the FAQ.
    fileFormat :: Prelude.Maybe FaqFileFormat
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
-- 'name', 'faqSummary_name' - The name that you assigned the FAQ when you created or updated the FAQ.
--
-- 'status', 'faqSummary_status' - The current status of the FAQ. When the status is @ACTIVE@ the FAQ is
-- ready for use.
--
-- 'id', 'faqSummary_id' - The unique identifier of the FAQ.
--
-- 'languageCode', 'faqSummary_languageCode' - The code for a language. This shows a supported language for the FAQ
-- document as part of the summary information for FAQs. English is
-- supported by default. For more information on supported languages,
-- including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'createdAt', 'faqSummary_createdAt' - The UNIX datetime that the FAQ was added to the index.
--
-- 'updatedAt', 'faqSummary_updatedAt' - The UNIX datetime that the FAQ was last updated.
--
-- 'fileFormat', 'faqSummary_fileFormat' - The file type used to create the FAQ.
newFaqSummary ::
  FaqSummary
newFaqSummary =
  FaqSummary'
    { name = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      fileFormat = Prelude.Nothing
    }

-- | The name that you assigned the FAQ when you created or updated the FAQ.
faqSummary_name :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.Text)
faqSummary_name = Lens.lens (\FaqSummary' {name} -> name) (\s@FaqSummary' {} a -> s {name = a} :: FaqSummary)

-- | The current status of the FAQ. When the status is @ACTIVE@ the FAQ is
-- ready for use.
faqSummary_status :: Lens.Lens' FaqSummary (Prelude.Maybe FaqStatus)
faqSummary_status = Lens.lens (\FaqSummary' {status} -> status) (\s@FaqSummary' {} a -> s {status = a} :: FaqSummary)

-- | The unique identifier of the FAQ.
faqSummary_id :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.Text)
faqSummary_id = Lens.lens (\FaqSummary' {id} -> id) (\s@FaqSummary' {} a -> s {id = a} :: FaqSummary)

-- | The code for a language. This shows a supported language for the FAQ
-- document as part of the summary information for FAQs. English is
-- supported by default. For more information on supported languages,
-- including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
faqSummary_languageCode :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.Text)
faqSummary_languageCode = Lens.lens (\FaqSummary' {languageCode} -> languageCode) (\s@FaqSummary' {} a -> s {languageCode = a} :: FaqSummary)

-- | The UNIX datetime that the FAQ was added to the index.
faqSummary_createdAt :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.UTCTime)
faqSummary_createdAt = Lens.lens (\FaqSummary' {createdAt} -> createdAt) (\s@FaqSummary' {} a -> s {createdAt = a} :: FaqSummary) Prelude.. Lens.mapping Core._Time

-- | The UNIX datetime that the FAQ was last updated.
faqSummary_updatedAt :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.UTCTime)
faqSummary_updatedAt = Lens.lens (\FaqSummary' {updatedAt} -> updatedAt) (\s@FaqSummary' {} a -> s {updatedAt = a} :: FaqSummary) Prelude.. Lens.mapping Core._Time

-- | The file type used to create the FAQ.
faqSummary_fileFormat :: Lens.Lens' FaqSummary (Prelude.Maybe FaqFileFormat)
faqSummary_fileFormat = Lens.lens (\FaqSummary' {fileFormat} -> fileFormat) (\s@FaqSummary' {} a -> s {fileFormat = a} :: FaqSummary)

instance Core.FromJSON FaqSummary where
  parseJSON =
    Core.withObject
      "FaqSummary"
      ( \x ->
          FaqSummary'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "UpdatedAt")
            Prelude.<*> (x Core..:? "FileFormat")
      )

instance Prelude.Hashable FaqSummary where
  hashWithSalt _salt FaqSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` fileFormat

instance Prelude.NFData FaqSummary where
  rnf FaqSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf fileFormat
