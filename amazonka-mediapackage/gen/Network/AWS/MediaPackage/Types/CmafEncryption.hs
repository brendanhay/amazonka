{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaPackage.Types.CmafEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.CmafEncryption where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import qualified Network.AWS.Prelude as Prelude

-- | A Common Media Application Format (CMAF) encryption configuration.
--
-- /See:/ 'newCmafEncryption' smart constructor.
data CmafEncryption = CmafEncryption'
  { -- | Time (in seconds) between each encryption key rotation.
    keyRotationIntervalSeconds :: Prelude.Maybe Prelude.Int,
    spekeKeyProvider :: SpekeKeyProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CmafEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyRotationIntervalSeconds', 'cmafEncryption_keyRotationIntervalSeconds' - Time (in seconds) between each encryption key rotation.
--
-- 'spekeKeyProvider', 'cmafEncryption_spekeKeyProvider' - Undocumented member.
newCmafEncryption ::
  -- | 'spekeKeyProvider'
  SpekeKeyProvider ->
  CmafEncryption
newCmafEncryption pSpekeKeyProvider_ =
  CmafEncryption'
    { keyRotationIntervalSeconds =
        Prelude.Nothing,
      spekeKeyProvider = pSpekeKeyProvider_
    }

-- | Time (in seconds) between each encryption key rotation.
cmafEncryption_keyRotationIntervalSeconds :: Lens.Lens' CmafEncryption (Prelude.Maybe Prelude.Int)
cmafEncryption_keyRotationIntervalSeconds = Lens.lens (\CmafEncryption' {keyRotationIntervalSeconds} -> keyRotationIntervalSeconds) (\s@CmafEncryption' {} a -> s {keyRotationIntervalSeconds = a} :: CmafEncryption)

-- | Undocumented member.
cmafEncryption_spekeKeyProvider :: Lens.Lens' CmafEncryption SpekeKeyProvider
cmafEncryption_spekeKeyProvider = Lens.lens (\CmafEncryption' {spekeKeyProvider} -> spekeKeyProvider) (\s@CmafEncryption' {} a -> s {spekeKeyProvider = a} :: CmafEncryption)

instance Prelude.FromJSON CmafEncryption where
  parseJSON =
    Prelude.withObject
      "CmafEncryption"
      ( \x ->
          CmafEncryption'
            Prelude.<$> (x Prelude..:? "keyRotationIntervalSeconds")
            Prelude.<*> (x Prelude..: "spekeKeyProvider")
      )

instance Prelude.Hashable CmafEncryption

instance Prelude.NFData CmafEncryption

instance Prelude.ToJSON CmafEncryption where
  toJSON CmafEncryption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("keyRotationIntervalSeconds" Prelude..=)
              Prelude.<$> keyRotationIntervalSeconds,
            Prelude.Just
              ("spekeKeyProvider" Prelude..= spekeKeyProvider)
          ]
      )
