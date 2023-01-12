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
-- Module      : Amazonka.Textract.Types.DetectedSignature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.DetectedSignature where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that holds information regarding a detected signature on a
-- page.
--
-- /See:/ 'newDetectedSignature' smart constructor.
data DetectedSignature = DetectedSignature'
  { -- | The page a detected signature was found on.
    page :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectedSignature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'page', 'detectedSignature_page' - The page a detected signature was found on.
newDetectedSignature ::
  DetectedSignature
newDetectedSignature =
  DetectedSignature' {page = Prelude.Nothing}

-- | The page a detected signature was found on.
detectedSignature_page :: Lens.Lens' DetectedSignature (Prelude.Maybe Prelude.Natural)
detectedSignature_page = Lens.lens (\DetectedSignature' {page} -> page) (\s@DetectedSignature' {} a -> s {page = a} :: DetectedSignature)

instance Data.FromJSON DetectedSignature where
  parseJSON =
    Data.withObject
      "DetectedSignature"
      ( \x ->
          DetectedSignature' Prelude.<$> (x Data..:? "Page")
      )

instance Prelude.Hashable DetectedSignature where
  hashWithSalt _salt DetectedSignature' {..} =
    _salt `Prelude.hashWithSalt` page

instance Prelude.NFData DetectedSignature where
  rnf DetectedSignature' {..} = Prelude.rnf page
