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
-- Module      : Network.AWS.Kendra.Types.FaqSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.FaqSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.FaqFileFormat
import Network.AWS.Kendra.Types.FaqStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a frequently asked questions and answer
-- contained in an index.
--
-- /See:/ 'newFaqSummary' smart constructor.
data FaqSummary = FaqSummary'
  { -- | The current status of the FAQ. When the status is @ACTIVE@ the FAQ is
    -- ready for use.
    status :: Prelude.Maybe FaqStatus,
    -- | The code for a language. This shows a supported language for the FAQ
    -- document as part of the summary information for FAQs. English is
    -- supported by default. For more information on supported languages,
    -- including their codes, see
    -- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
    languageCode :: Prelude.Maybe Prelude.Text,
    -- | The UNIX datetime that the FAQ was added to the index.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The file type used to create the FAQ.
    fileFormat :: Prelude.Maybe FaqFileFormat,
    -- | The name that you assigned the FAQ when you created or updated the FAQ.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the FAQ.
    id :: Prelude.Maybe Prelude.Text,
    -- | The UNIX datetime that the FAQ was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX
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
-- 'status', 'faqSummary_status' - The current status of the FAQ. When the status is @ACTIVE@ the FAQ is
-- ready for use.
--
-- 'languageCode', 'faqSummary_languageCode' - The code for a language. This shows a supported language for the FAQ
-- document as part of the summary information for FAQs. English is
-- supported by default. For more information on supported languages,
-- including their codes, see
-- <https://docs.aws.amazon.com/kendra/latest/dg/in-adding-languages.html Adding documents in languages other than English>.
--
-- 'createdAt', 'faqSummary_createdAt' - The UNIX datetime that the FAQ was added to the index.
--
-- 'fileFormat', 'faqSummary_fileFormat' - The file type used to create the FAQ.
--
-- 'name', 'faqSummary_name' - The name that you assigned the FAQ when you created or updated the FAQ.
--
-- 'id', 'faqSummary_id' - The unique identifier of the FAQ.
--
-- 'updatedAt', 'faqSummary_updatedAt' - The UNIX datetime that the FAQ was last updated.
newFaqSummary ::
  FaqSummary
newFaqSummary =
  FaqSummary'
    { status = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      fileFormat = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The current status of the FAQ. When the status is @ACTIVE@ the FAQ is
-- ready for use.
faqSummary_status :: Lens.Lens' FaqSummary (Prelude.Maybe FaqStatus)
faqSummary_status = Lens.lens (\FaqSummary' {status} -> status) (\s@FaqSummary' {} a -> s {status = a} :: FaqSummary)

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

-- | The file type used to create the FAQ.
faqSummary_fileFormat :: Lens.Lens' FaqSummary (Prelude.Maybe FaqFileFormat)
faqSummary_fileFormat = Lens.lens (\FaqSummary' {fileFormat} -> fileFormat) (\s@FaqSummary' {} a -> s {fileFormat = a} :: FaqSummary)

-- | The name that you assigned the FAQ when you created or updated the FAQ.
faqSummary_name :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.Text)
faqSummary_name = Lens.lens (\FaqSummary' {name} -> name) (\s@FaqSummary' {} a -> s {name = a} :: FaqSummary)

-- | The unique identifier of the FAQ.
faqSummary_id :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.Text)
faqSummary_id = Lens.lens (\FaqSummary' {id} -> id) (\s@FaqSummary' {} a -> s {id = a} :: FaqSummary)

-- | The UNIX datetime that the FAQ was last updated.
faqSummary_updatedAt :: Lens.Lens' FaqSummary (Prelude.Maybe Prelude.UTCTime)
faqSummary_updatedAt = Lens.lens (\FaqSummary' {updatedAt} -> updatedAt) (\s@FaqSummary' {} a -> s {updatedAt = a} :: FaqSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON FaqSummary where
  parseJSON =
    Core.withObject
      "FaqSummary"
      ( \x ->
          FaqSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "FileFormat")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "UpdatedAt")
      )

instance Prelude.Hashable FaqSummary

instance Prelude.NFData FaqSummary
