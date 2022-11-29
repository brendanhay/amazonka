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
-- Module      : Amazonka.MediaPackageVOD.Types.MssEncryption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.MssEncryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaPackageVOD.Types.SpekeKeyProvider
import qualified Amazonka.Prelude as Prelude

-- | A Microsoft Smooth Streaming (MSS) encryption configuration.
--
-- /See:/ 'newMssEncryption' smart constructor.
data MssEncryption = MssEncryption'
  { spekeKeyProvider :: SpekeKeyProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.FromJSON MssEncryption where
  parseJSON =
    Core.withObject
      "MssEncryption"
      ( \x ->
          MssEncryption'
            Prelude.<$> (x Core..: "spekeKeyProvider")
      )

instance Prelude.Hashable MssEncryption where
  hashWithSalt _salt MssEncryption' {..} =
    _salt `Prelude.hashWithSalt` spekeKeyProvider

instance Prelude.NFData MssEncryption where
  rnf MssEncryption' {..} = Prelude.rnf spekeKeyProvider

instance Core.ToJSON MssEncryption where
  toJSON MssEncryption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("spekeKeyProvider" Core..= spekeKeyProvider)
          ]
      )
