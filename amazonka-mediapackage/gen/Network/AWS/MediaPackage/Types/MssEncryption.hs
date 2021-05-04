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
-- Module      : Network.AWS.MediaPackage.Types.MssEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.MssEncryption where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import qualified Network.AWS.Prelude as Prelude

-- | A Microsoft Smooth Streaming (MSS) encryption configuration.
--
-- /See:/ 'newMssEncryption' smart constructor.
data MssEncryption = MssEncryption'
  { spekeKeyProvider :: SpekeKeyProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MssEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spekeKeyProvider', 'mssEncryption_spekeKeyProvider' - Undocumented member.
newMssEncryption ::
  -- | 'spekeKeyProvider'
  SpekeKeyProvider ->
  MssEncryption
newMssEncryption pSpekeKeyProvider_ =
  MssEncryption'
    { spekeKeyProvider =
        pSpekeKeyProvider_
    }

-- | Undocumented member.
mssEncryption_spekeKeyProvider :: Lens.Lens' MssEncryption SpekeKeyProvider
mssEncryption_spekeKeyProvider = Lens.lens (\MssEncryption' {spekeKeyProvider} -> spekeKeyProvider) (\s@MssEncryption' {} a -> s {spekeKeyProvider = a} :: MssEncryption)

instance Prelude.FromJSON MssEncryption where
  parseJSON =
    Prelude.withObject
      "MssEncryption"
      ( \x ->
          MssEncryption'
            Prelude.<$> (x Prelude..: "spekeKeyProvider")
      )

instance Prelude.Hashable MssEncryption

instance Prelude.NFData MssEncryption

instance Prelude.ToJSON MssEncryption where
  toJSON MssEncryption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("spekeKeyProvider" Prelude..= spekeKeyProvider)
          ]
      )
