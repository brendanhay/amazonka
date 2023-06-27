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
-- Module      : Amazonka.PaymentCryptography.Types.ImportTr31KeyBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptography.Types.ImportTr31KeyBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameter information for key material import using TR-31 standard.
--
-- /See:/ 'newImportTr31KeyBlock' smart constructor.
data ImportTr31KeyBlock = ImportTr31KeyBlock'
  { -- | The TR-34 wrapped key block to import.
    wrappedKeyBlock :: Prelude.Text,
    -- | The @KeyARN@ of the key that will decrypt or unwrap a TR-31 key block
    -- during import.
    wrappingKeyIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportTr31KeyBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wrappedKeyBlock', 'importTr31KeyBlock_wrappedKeyBlock' - The TR-34 wrapped key block to import.
--
-- 'wrappingKeyIdentifier', 'importTr31KeyBlock_wrappingKeyIdentifier' - The @KeyARN@ of the key that will decrypt or unwrap a TR-31 key block
-- during import.
newImportTr31KeyBlock ::
  -- | 'wrappedKeyBlock'
  Prelude.Text ->
  -- | 'wrappingKeyIdentifier'
  Prelude.Text ->
  ImportTr31KeyBlock
newImportTr31KeyBlock
  pWrappedKeyBlock_
  pWrappingKeyIdentifier_ =
    ImportTr31KeyBlock'
      { wrappedKeyBlock =
          pWrappedKeyBlock_,
        wrappingKeyIdentifier = pWrappingKeyIdentifier_
      }

-- | The TR-34 wrapped key block to import.
importTr31KeyBlock_wrappedKeyBlock :: Lens.Lens' ImportTr31KeyBlock Prelude.Text
importTr31KeyBlock_wrappedKeyBlock = Lens.lens (\ImportTr31KeyBlock' {wrappedKeyBlock} -> wrappedKeyBlock) (\s@ImportTr31KeyBlock' {} a -> s {wrappedKeyBlock = a} :: ImportTr31KeyBlock)

-- | The @KeyARN@ of the key that will decrypt or unwrap a TR-31 key block
-- during import.
importTr31KeyBlock_wrappingKeyIdentifier :: Lens.Lens' ImportTr31KeyBlock Prelude.Text
importTr31KeyBlock_wrappingKeyIdentifier = Lens.lens (\ImportTr31KeyBlock' {wrappingKeyIdentifier} -> wrappingKeyIdentifier) (\s@ImportTr31KeyBlock' {} a -> s {wrappingKeyIdentifier = a} :: ImportTr31KeyBlock)

instance Prelude.Hashable ImportTr31KeyBlock where
  hashWithSalt _salt ImportTr31KeyBlock' {..} =
    _salt
      `Prelude.hashWithSalt` wrappedKeyBlock
      `Prelude.hashWithSalt` wrappingKeyIdentifier

instance Prelude.NFData ImportTr31KeyBlock where
  rnf ImportTr31KeyBlock' {..} =
    Prelude.rnf wrappedKeyBlock
      `Prelude.seq` Prelude.rnf wrappingKeyIdentifier

instance Data.ToJSON ImportTr31KeyBlock where
  toJSON ImportTr31KeyBlock' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WrappedKeyBlock" Data..= wrappedKeyBlock),
            Prelude.Just
              ( "WrappingKeyIdentifier"
                  Data..= wrappingKeyIdentifier
              )
          ]
      )
