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
-- Module      : Amazonka.Textract.Types.UndetectedSignature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.UndetectedSignature where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing information about an undetected signature on a
-- page where it was expected but not found.
--
-- /See:/ 'newUndetectedSignature' smart constructor.
data UndetectedSignature = UndetectedSignature'
  { -- | The page where a signature was expected but not found.
    page :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UndetectedSignature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'page', 'undetectedSignature_page' - The page where a signature was expected but not found.
newUndetectedSignature ::
  UndetectedSignature
newUndetectedSignature =
  UndetectedSignature' {page = Prelude.Nothing}

-- | The page where a signature was expected but not found.
undetectedSignature_page :: Lens.Lens' UndetectedSignature (Prelude.Maybe Prelude.Natural)
undetectedSignature_page = Lens.lens (\UndetectedSignature' {page} -> page) (\s@UndetectedSignature' {} a -> s {page = a} :: UndetectedSignature)

instance Data.FromJSON UndetectedSignature where
  parseJSON =
    Data.withObject
      "UndetectedSignature"
      ( \x ->
          UndetectedSignature' Prelude.<$> (x Data..:? "Page")
      )

instance Prelude.Hashable UndetectedSignature where
  hashWithSalt _salt UndetectedSignature' {..} =
    _salt `Prelude.hashWithSalt` page

instance Prelude.NFData UndetectedSignature where
  rnf UndetectedSignature' {..} = Prelude.rnf page
