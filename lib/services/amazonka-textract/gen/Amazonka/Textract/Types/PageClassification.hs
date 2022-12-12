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
-- Module      : Amazonka.Textract.Types.PageClassification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.PageClassification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.Prediction

-- | The class assigned to a Page object detected in an input document.
-- Contains information regarding the predicted type\/class of a
-- document\'s page and the page number that the Page object was detected
-- on.
--
-- /See:/ 'newPageClassification' smart constructor.
data PageClassification = PageClassification'
  { -- | The class, or document type, assigned to a detected Page object. The
    -- class, or document type, assigned to a detected Page object.
    pageType :: [Prediction],
    -- | The page number the value was detected on, relative to Amazon
    -- Textract\'s starting position.
    pageNumber :: [Prediction]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PageClassification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageType', 'pageClassification_pageType' - The class, or document type, assigned to a detected Page object. The
-- class, or document type, assigned to a detected Page object.
--
-- 'pageNumber', 'pageClassification_pageNumber' - The page number the value was detected on, relative to Amazon
-- Textract\'s starting position.
newPageClassification ::
  PageClassification
newPageClassification =
  PageClassification'
    { pageType = Prelude.mempty,
      pageNumber = Prelude.mempty
    }

-- | The class, or document type, assigned to a detected Page object. The
-- class, or document type, assigned to a detected Page object.
pageClassification_pageType :: Lens.Lens' PageClassification [Prediction]
pageClassification_pageType = Lens.lens (\PageClassification' {pageType} -> pageType) (\s@PageClassification' {} a -> s {pageType = a} :: PageClassification) Prelude.. Lens.coerced

-- | The page number the value was detected on, relative to Amazon
-- Textract\'s starting position.
pageClassification_pageNumber :: Lens.Lens' PageClassification [Prediction]
pageClassification_pageNumber = Lens.lens (\PageClassification' {pageNumber} -> pageNumber) (\s@PageClassification' {} a -> s {pageNumber = a} :: PageClassification) Prelude.. Lens.coerced

instance Data.FromJSON PageClassification where
  parseJSON =
    Data.withObject
      "PageClassification"
      ( \x ->
          PageClassification'
            Prelude.<$> (x Data..:? "PageType" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PageNumber" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PageClassification where
  hashWithSalt _salt PageClassification' {..} =
    _salt `Prelude.hashWithSalt` pageType
      `Prelude.hashWithSalt` pageNumber

instance Prelude.NFData PageClassification where
  rnf PageClassification' {..} =
    Prelude.rnf pageType
      `Prelude.seq` Prelude.rnf pageNumber
