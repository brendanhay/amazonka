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
-- Module      : Amazonka.MediaPackageV2.Types.EncryptionMethod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.EncryptionMethod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.CmafEncryptionMethod
import Amazonka.MediaPackageV2.Types.TsEncryptionMethod
import qualified Amazonka.Prelude as Prelude

-- | The encryption type.
--
-- /See:/ 'newEncryptionMethod' smart constructor.
data EncryptionMethod = EncryptionMethod'
  { -- | The encryption method to use.
    cmafEncryptionMethod :: Prelude.Maybe CmafEncryptionMethod,
    -- | The encryption method to use.
    tsEncryptionMethod :: Prelude.Maybe TsEncryptionMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cmafEncryptionMethod', 'encryptionMethod_cmafEncryptionMethod' - The encryption method to use.
--
-- 'tsEncryptionMethod', 'encryptionMethod_tsEncryptionMethod' - The encryption method to use.
newEncryptionMethod ::
  EncryptionMethod
newEncryptionMethod =
  EncryptionMethod'
    { cmafEncryptionMethod =
        Prelude.Nothing,
      tsEncryptionMethod = Prelude.Nothing
    }

-- | The encryption method to use.
encryptionMethod_cmafEncryptionMethod :: Lens.Lens' EncryptionMethod (Prelude.Maybe CmafEncryptionMethod)
encryptionMethod_cmafEncryptionMethod = Lens.lens (\EncryptionMethod' {cmafEncryptionMethod} -> cmafEncryptionMethod) (\s@EncryptionMethod' {} a -> s {cmafEncryptionMethod = a} :: EncryptionMethod)

-- | The encryption method to use.
encryptionMethod_tsEncryptionMethod :: Lens.Lens' EncryptionMethod (Prelude.Maybe TsEncryptionMethod)
encryptionMethod_tsEncryptionMethod = Lens.lens (\EncryptionMethod' {tsEncryptionMethod} -> tsEncryptionMethod) (\s@EncryptionMethod' {} a -> s {tsEncryptionMethod = a} :: EncryptionMethod)

instance Data.FromJSON EncryptionMethod where
  parseJSON =
    Data.withObject
      "EncryptionMethod"
      ( \x ->
          EncryptionMethod'
            Prelude.<$> (x Data..:? "CmafEncryptionMethod")
            Prelude.<*> (x Data..:? "TsEncryptionMethod")
      )

instance Prelude.Hashable EncryptionMethod where
  hashWithSalt _salt EncryptionMethod' {..} =
    _salt
      `Prelude.hashWithSalt` cmafEncryptionMethod
      `Prelude.hashWithSalt` tsEncryptionMethod

instance Prelude.NFData EncryptionMethod where
  rnf EncryptionMethod' {..} =
    Prelude.rnf cmafEncryptionMethod
      `Prelude.seq` Prelude.rnf tsEncryptionMethod

instance Data.ToJSON EncryptionMethod where
  toJSON EncryptionMethod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CmafEncryptionMethod" Data..=)
              Prelude.<$> cmafEncryptionMethod,
            ("TsEncryptionMethod" Data..=)
              Prelude.<$> tsEncryptionMethod
          ]
      )
