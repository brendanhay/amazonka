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
-- Module      : Amazonka.PaymentCryptography.Types.ExportKeyMaterial
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.ExportKeyMaterial where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptography.Types.ExportTr31KeyBlock
import Amazonka.PaymentCryptography.Types.ExportTr34KeyBlock
import qualified Amazonka.Prelude as Prelude

-- | Parameter information for key material export from Amazon Web Services
-- Payment Cryptography.
--
-- /See:/ 'newExportKeyMaterial' smart constructor.
data ExportKeyMaterial = ExportKeyMaterial'
  { -- | Parameter information for key material export using TR-31 standard.
    tr31KeyBlock :: Prelude.Maybe ExportTr31KeyBlock,
    -- | Parameter information for key material export using TR-34 standard.
    tr34KeyBlock :: Prelude.Maybe ExportTr34KeyBlock
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportKeyMaterial' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tr31KeyBlock', 'exportKeyMaterial_tr31KeyBlock' - Parameter information for key material export using TR-31 standard.
--
-- 'tr34KeyBlock', 'exportKeyMaterial_tr34KeyBlock' - Parameter information for key material export using TR-34 standard.
newExportKeyMaterial ::
  ExportKeyMaterial
newExportKeyMaterial =
  ExportKeyMaterial'
    { tr31KeyBlock = Prelude.Nothing,
      tr34KeyBlock = Prelude.Nothing
    }

-- | Parameter information for key material export using TR-31 standard.
exportKeyMaterial_tr31KeyBlock :: Lens.Lens' ExportKeyMaterial (Prelude.Maybe ExportTr31KeyBlock)
exportKeyMaterial_tr31KeyBlock = Lens.lens (\ExportKeyMaterial' {tr31KeyBlock} -> tr31KeyBlock) (\s@ExportKeyMaterial' {} a -> s {tr31KeyBlock = a} :: ExportKeyMaterial)

-- | Parameter information for key material export using TR-34 standard.
exportKeyMaterial_tr34KeyBlock :: Lens.Lens' ExportKeyMaterial (Prelude.Maybe ExportTr34KeyBlock)
exportKeyMaterial_tr34KeyBlock = Lens.lens (\ExportKeyMaterial' {tr34KeyBlock} -> tr34KeyBlock) (\s@ExportKeyMaterial' {} a -> s {tr34KeyBlock = a} :: ExportKeyMaterial)

instance Prelude.Hashable ExportKeyMaterial where
  hashWithSalt _salt ExportKeyMaterial' {..} =
    _salt
      `Prelude.hashWithSalt` tr31KeyBlock
      `Prelude.hashWithSalt` tr34KeyBlock

instance Prelude.NFData ExportKeyMaterial where
  rnf ExportKeyMaterial' {..} =
    Prelude.rnf tr31KeyBlock
      `Prelude.seq` Prelude.rnf tr34KeyBlock

instance Data.ToJSON ExportKeyMaterial where
  toJSON ExportKeyMaterial' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tr31KeyBlock" Data..=) Prelude.<$> tr31KeyBlock,
            ("Tr34KeyBlock" Data..=) Prelude.<$> tr34KeyBlock
          ]
      )
