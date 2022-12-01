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
-- Module      : Amazonka.GuardDuty.Types.ThreatsDetectedItemCount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.ThreatsDetectedItemCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains total number of infected files.
--
-- /See:/ 'newThreatsDetectedItemCount' smart constructor.
data ThreatsDetectedItemCount = ThreatsDetectedItemCount'
  { -- | Total number of infected files.
    files :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThreatsDetectedItemCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'files', 'threatsDetectedItemCount_files' - Total number of infected files.
newThreatsDetectedItemCount ::
  ThreatsDetectedItemCount
newThreatsDetectedItemCount =
  ThreatsDetectedItemCount' {files = Prelude.Nothing}

-- | Total number of infected files.
threatsDetectedItemCount_files :: Lens.Lens' ThreatsDetectedItemCount (Prelude.Maybe Prelude.Int)
threatsDetectedItemCount_files = Lens.lens (\ThreatsDetectedItemCount' {files} -> files) (\s@ThreatsDetectedItemCount' {} a -> s {files = a} :: ThreatsDetectedItemCount)

instance Core.FromJSON ThreatsDetectedItemCount where
  parseJSON =
    Core.withObject
      "ThreatsDetectedItemCount"
      ( \x ->
          ThreatsDetectedItemCount'
            Prelude.<$> (x Core..:? "files")
      )

instance Prelude.Hashable ThreatsDetectedItemCount where
  hashWithSalt _salt ThreatsDetectedItemCount' {..} =
    _salt `Prelude.hashWithSalt` files

instance Prelude.NFData ThreatsDetectedItemCount where
  rnf ThreatsDetectedItemCount' {..} = Prelude.rnf files
