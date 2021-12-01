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
-- Module      : Amazonka.MediaPackageVOD.Types.HlsEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.HlsEncryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaPackageVOD.Types.EncryptionMethod
import Amazonka.MediaPackageVOD.Types.SpekeKeyProvider
import qualified Amazonka.Prelude as Prelude

-- | An HTTP Live Streaming (HLS) encryption configuration.
--
-- /See:/ 'newHlsEncryption' smart constructor.
data HlsEncryption = HlsEncryption'
  { -- | The encryption method to use.
    encryptionMethod :: Prelude.Maybe EncryptionMethod,
    -- | A constant initialization vector for encryption (optional). When not
    -- specified the initialization vector will be periodically rotated.
    constantInitializationVector :: Prelude.Maybe Prelude.Text,
    spekeKeyProvider :: SpekeKeyProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionMethod', 'hlsEncryption_encryptionMethod' - The encryption method to use.
--
-- 'constantInitializationVector', 'hlsEncryption_constantInitializationVector' - A constant initialization vector for encryption (optional). When not
-- specified the initialization vector will be periodically rotated.
--
-- 'spekeKeyProvider', 'hlsEncryption_spekeKeyProvider' - Undocumented member.
newHlsEncryption ::
  -- | 'spekeKeyProvider'
  SpekeKeyProvider ->
  HlsEncryption
newHlsEncryption pSpekeKeyProvider_ =
  HlsEncryption'
    { encryptionMethod = Prelude.Nothing,
      constantInitializationVector = Prelude.Nothing,
      spekeKeyProvider = pSpekeKeyProvider_
    }

-- | The encryption method to use.
hlsEncryption_encryptionMethod :: Lens.Lens' HlsEncryption (Prelude.Maybe EncryptionMethod)
hlsEncryption_encryptionMethod = Lens.lens (\HlsEncryption' {encryptionMethod} -> encryptionMethod) (\s@HlsEncryption' {} a -> s {encryptionMethod = a} :: HlsEncryption)

-- | A constant initialization vector for encryption (optional). When not
-- specified the initialization vector will be periodically rotated.
hlsEncryption_constantInitializationVector :: Lens.Lens' HlsEncryption (Prelude.Maybe Prelude.Text)
hlsEncryption_constantInitializationVector = Lens.lens (\HlsEncryption' {constantInitializationVector} -> constantInitializationVector) (\s@HlsEncryption' {} a -> s {constantInitializationVector = a} :: HlsEncryption)

-- | Undocumented member.
hlsEncryption_spekeKeyProvider :: Lens.Lens' HlsEncryption SpekeKeyProvider
hlsEncryption_spekeKeyProvider = Lens.lens (\HlsEncryption' {spekeKeyProvider} -> spekeKeyProvider) (\s@HlsEncryption' {} a -> s {spekeKeyProvider = a} :: HlsEncryption)

instance Core.FromJSON HlsEncryption where
  parseJSON =
    Core.withObject
      "HlsEncryption"
      ( \x ->
          HlsEncryption'
            Prelude.<$> (x Core..:? "encryptionMethod")
            Prelude.<*> (x Core..:? "constantInitializationVector")
            Prelude.<*> (x Core..: "spekeKeyProvider")
      )

instance Prelude.Hashable HlsEncryption where
  hashWithSalt salt' HlsEncryption' {..} =
    salt' `Prelude.hashWithSalt` spekeKeyProvider
      `Prelude.hashWithSalt` constantInitializationVector
      `Prelude.hashWithSalt` encryptionMethod

instance Prelude.NFData HlsEncryption where
  rnf HlsEncryption' {..} =
    Prelude.rnf encryptionMethod
      `Prelude.seq` Prelude.rnf spekeKeyProvider
      `Prelude.seq` Prelude.rnf constantInitializationVector

instance Core.ToJSON HlsEncryption where
  toJSON HlsEncryption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("encryptionMethod" Core..=)
              Prelude.<$> encryptionMethod,
            ("constantInitializationVector" Core..=)
              Prelude.<$> constantInitializationVector,
            Prelude.Just
              ("spekeKeyProvider" Core..= spekeKeyProvider)
          ]
      )
