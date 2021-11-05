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
-- Module      : Network.AWS.WellArchitected.Types.LensReviewReport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WellArchitected.Types.LensReviewReport where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A report of a lens review.
--
-- /See:/ 'newLensReviewReport' smart constructor.
data LensReviewReport = LensReviewReport'
  { lensAlias :: Prelude.Maybe Prelude.Text,
    base64String :: Prelude.Maybe Prelude.Text
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
-- 'lensAlias', 'lensReviewReport_lensAlias' - Undocumented member.
--
-- 'base64String', 'lensReviewReport_base64String' - Undocumented member.
newLensReviewReport ::
  LensReviewReport
newLensReviewReport =
  LensReviewReport'
    { lensAlias = Prelude.Nothing,
      base64String = Prelude.Nothing
    }

-- | Undocumented member.
lensReviewReport_lensAlias :: Lens.Lens' LensReviewReport (Prelude.Maybe Prelude.Text)
lensReviewReport_lensAlias = Lens.lens (\LensReviewReport' {lensAlias} -> lensAlias) (\s@LensReviewReport' {} a -> s {lensAlias = a} :: LensReviewReport)

-- | Undocumented member.
lensReviewReport_base64String :: Lens.Lens' LensReviewReport (Prelude.Maybe Prelude.Text)
lensReviewReport_base64String = Lens.lens (\LensReviewReport' {base64String} -> base64String) (\s@LensReviewReport' {} a -> s {base64String = a} :: LensReviewReport)

instance Core.FromJSON LensReviewReport where
  parseJSON =
    Core.withObject
      "LensReviewReport"
      ( \x ->
          LensReviewReport'
            Prelude.<$> (x Core..:? "LensAlias")
            Prelude.<*> (x Core..:? "Base64String")
      )

instance Prelude.Hashable LensReviewReport

instance Prelude.NFData LensReviewReport
