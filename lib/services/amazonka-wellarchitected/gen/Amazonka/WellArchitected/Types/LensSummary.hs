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
-- Module      : Amazonka.WellArchitected.Types.LensSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A lens summary of a lens.
--
-- /See:/ 'newLensSummary' smart constructor.
data LensSummary = LensSummary'
  { lensAlias :: Prelude.Maybe Prelude.Text,
    lensName :: Prelude.Maybe Prelude.Text,
    -- | The version of the lens.
    lensVersion :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LensSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensAlias', 'lensSummary_lensAlias' - Undocumented member.
--
-- 'lensName', 'lensSummary_lensName' - Undocumented member.
--
-- 'lensVersion', 'lensSummary_lensVersion' - The version of the lens.
--
-- 'description', 'lensSummary_description' - Undocumented member.
newLensSummary ::
  LensSummary
newLensSummary =
  LensSummary'
    { lensAlias = Prelude.Nothing,
      lensName = Prelude.Nothing,
      lensVersion = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Undocumented member.
lensSummary_lensAlias :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.Text)
lensSummary_lensAlias = Lens.lens (\LensSummary' {lensAlias} -> lensAlias) (\s@LensSummary' {} a -> s {lensAlias = a} :: LensSummary)

-- | Undocumented member.
lensSummary_lensName :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.Text)
lensSummary_lensName = Lens.lens (\LensSummary' {lensName} -> lensName) (\s@LensSummary' {} a -> s {lensName = a} :: LensSummary)

-- | The version of the lens.
lensSummary_lensVersion :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.Text)
lensSummary_lensVersion = Lens.lens (\LensSummary' {lensVersion} -> lensVersion) (\s@LensSummary' {} a -> s {lensVersion = a} :: LensSummary)

-- | Undocumented member.
lensSummary_description :: Lens.Lens' LensSummary (Prelude.Maybe Prelude.Text)
lensSummary_description = Lens.lens (\LensSummary' {description} -> description) (\s@LensSummary' {} a -> s {description = a} :: LensSummary)

instance Core.FromJSON LensSummary where
  parseJSON =
    Core.withObject
      "LensSummary"
      ( \x ->
          LensSummary'
            Prelude.<$> (x Core..:? "LensAlias")
            Prelude.<*> (x Core..:? "LensName")
            Prelude.<*> (x Core..:? "LensVersion")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable LensSummary where
  hashWithSalt salt' LensSummary' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lensVersion
      `Prelude.hashWithSalt` lensName
      `Prelude.hashWithSalt` lensAlias

instance Prelude.NFData LensSummary where
  rnf LensSummary' {..} =
    Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lensVersion
      `Prelude.seq` Prelude.rnf lensName
