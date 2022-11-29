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
-- Module      : Amazonka.Textract.Types.Warning
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.Warning where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A warning about an issue that occurred during asynchronous text analysis
-- (StartDocumentAnalysis) or asynchronous document text detection
-- (StartDocumentTextDetection).
--
-- /See:/ 'newWarning' smart constructor.
data Warning = Warning'
  { -- | The error code for the warning.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | A list of the pages that the warning applies to.
    pages :: Prelude.Maybe [Prelude.Natural]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Warning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'warning_errorCode' - The error code for the warning.
--
-- 'pages', 'warning_pages' - A list of the pages that the warning applies to.
newWarning ::
  Warning
newWarning =
  Warning'
    { errorCode = Prelude.Nothing,
      pages = Prelude.Nothing
    }

-- | The error code for the warning.
warning_errorCode :: Lens.Lens' Warning (Prelude.Maybe Prelude.Text)
warning_errorCode = Lens.lens (\Warning' {errorCode} -> errorCode) (\s@Warning' {} a -> s {errorCode = a} :: Warning)

-- | A list of the pages that the warning applies to.
warning_pages :: Lens.Lens' Warning (Prelude.Maybe [Prelude.Natural])
warning_pages = Lens.lens (\Warning' {pages} -> pages) (\s@Warning' {} a -> s {pages = a} :: Warning) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Warning where
  parseJSON =
    Core.withObject
      "Warning"
      ( \x ->
          Warning'
            Prelude.<$> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "Pages" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Warning where
  hashWithSalt _salt Warning' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` pages

instance Prelude.NFData Warning where
  rnf Warning' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf pages
