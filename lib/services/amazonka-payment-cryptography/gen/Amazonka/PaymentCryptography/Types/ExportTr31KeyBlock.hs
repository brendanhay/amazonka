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
-- Module      : Amazonka.PaymentCryptography.Types.ExportTr31KeyBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.ExportTr31KeyBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameter information for key material export using TR-31 standard.
--
-- /See:/ 'newExportTr31KeyBlock' smart constructor.
data ExportTr31KeyBlock = ExportTr31KeyBlock'
  { -- | The @KeyARN@ of the the wrapping key. This key encrypts or wraps the key
    -- under export for TR-31 key block generation.
    wrappingKeyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportTr31KeyBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wrappingKeyIdentifier', 'exportTr31KeyBlock_wrappingKeyIdentifier' - The @KeyARN@ of the the wrapping key. This key encrypts or wraps the key
-- under export for TR-31 key block generation.
newExportTr31KeyBlock ::
  -- | 'wrappingKeyIdentifier'
  Prelude.Text ->
  ExportTr31KeyBlock
newExportTr31KeyBlock pWrappingKeyIdentifier_ =
  ExportTr31KeyBlock'
    { wrappingKeyIdentifier =
        pWrappingKeyIdentifier_
    }

-- | The @KeyARN@ of the the wrapping key. This key encrypts or wraps the key
-- under export for TR-31 key block generation.
exportTr31KeyBlock_wrappingKeyIdentifier :: Lens.Lens' ExportTr31KeyBlock Prelude.Text
exportTr31KeyBlock_wrappingKeyIdentifier = Lens.lens (\ExportTr31KeyBlock' {wrappingKeyIdentifier} -> wrappingKeyIdentifier) (\s@ExportTr31KeyBlock' {} a -> s {wrappingKeyIdentifier = a} :: ExportTr31KeyBlock)

instance Prelude.Hashable ExportTr31KeyBlock where
  hashWithSalt _salt ExportTr31KeyBlock' {..} =
    _salt `Prelude.hashWithSalt` wrappingKeyIdentifier

instance Prelude.NFData ExportTr31KeyBlock where
  rnf ExportTr31KeyBlock' {..} =
    Prelude.rnf wrappingKeyIdentifier

instance Data.ToJSON ExportTr31KeyBlock where
  toJSON ExportTr31KeyBlock' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "WrappingKeyIdentifier"
                  Data..= wrappingKeyIdentifier
              )
          ]
      )
