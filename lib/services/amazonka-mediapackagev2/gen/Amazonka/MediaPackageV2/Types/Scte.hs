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
-- Module      : Amazonka.MediaPackageV2.Types.Scte
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.Scte where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.ScteFilter
import qualified Amazonka.Prelude as Prelude

-- | The SCTE configuration.
--
-- /See:/ 'newScte' smart constructor.
data Scte = Scte'
  { -- | The SCTE-35 message types that you want to be treated as ad markers in
    -- the output.
    scteFilter :: Prelude.Maybe [ScteFilter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scte' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scteFilter', 'scte_scteFilter' - The SCTE-35 message types that you want to be treated as ad markers in
-- the output.
newScte ::
  Scte
newScte = Scte' {scteFilter = Prelude.Nothing}

-- | The SCTE-35 message types that you want to be treated as ad markers in
-- the output.
scte_scteFilter :: Lens.Lens' Scte (Prelude.Maybe [ScteFilter])
scte_scteFilter = Lens.lens (\Scte' {scteFilter} -> scteFilter) (\s@Scte' {} a -> s {scteFilter = a} :: Scte) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Scte where
  parseJSON =
    Data.withObject
      "Scte"
      ( \x ->
          Scte'
            Prelude.<$> (x Data..:? "ScteFilter" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Scte where
  hashWithSalt _salt Scte' {..} =
    _salt `Prelude.hashWithSalt` scteFilter

instance Prelude.NFData Scte where
  rnf Scte' {..} = Prelude.rnf scteFilter

instance Data.ToJSON Scte where
  toJSON Scte' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ScteFilter" Data..=) Prelude.<$> scteFilter]
      )
