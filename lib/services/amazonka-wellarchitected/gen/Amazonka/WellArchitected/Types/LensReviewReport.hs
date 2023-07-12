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
-- Module      : Amazonka.WellArchitected.Types.LensReviewReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensReviewReport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A report of a lens review.
--
-- /See:/ 'newLensReviewReport' smart constructor.
data LensReviewReport = LensReviewReport'
  { base64String :: Prelude.Maybe Prelude.Text,
    lensAlias :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LensReviewReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'base64String', 'lensReviewReport_base64String' - Undocumented member.
--
-- 'lensAlias', 'lensReviewReport_lensAlias' - Undocumented member.
--
-- 'lensArn', 'lensReviewReport_lensArn' - The ARN for the lens.
newLensReviewReport ::
  LensReviewReport
newLensReviewReport =
  LensReviewReport'
    { base64String = Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      lensArn = Prelude.Nothing
    }

-- | Undocumented member.
lensReviewReport_base64String :: Lens.Lens' LensReviewReport (Prelude.Maybe Prelude.Text)
lensReviewReport_base64String = Lens.lens (\LensReviewReport' {base64String} -> base64String) (\s@LensReviewReport' {} a -> s {base64String = a} :: LensReviewReport)

-- | Undocumented member.
lensReviewReport_lensAlias :: Lens.Lens' LensReviewReport (Prelude.Maybe Prelude.Text)
lensReviewReport_lensAlias = Lens.lens (\LensReviewReport' {lensAlias} -> lensAlias) (\s@LensReviewReport' {} a -> s {lensAlias = a} :: LensReviewReport)

-- | The ARN for the lens.
lensReviewReport_lensArn :: Lens.Lens' LensReviewReport (Prelude.Maybe Prelude.Text)
lensReviewReport_lensArn = Lens.lens (\LensReviewReport' {lensArn} -> lensArn) (\s@LensReviewReport' {} a -> s {lensArn = a} :: LensReviewReport)

instance Data.FromJSON LensReviewReport where
  parseJSON =
    Data.withObject
      "LensReviewReport"
      ( \x ->
          LensReviewReport'
            Prelude.<$> (x Data..:? "Base64String")
            Prelude.<*> (x Data..:? "LensAlias")
            Prelude.<*> (x Data..:? "LensArn")
      )

instance Prelude.Hashable LensReviewReport where
  hashWithSalt _salt LensReviewReport' {..} =
    _salt
      `Prelude.hashWithSalt` base64String
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` lensArn

instance Prelude.NFData LensReviewReport where
  rnf LensReviewReport' {..} =
    Prelude.rnf base64String
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf lensArn
