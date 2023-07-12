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
-- Module      : Amazonka.MediaPackage.Types.MssEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.MssEncryption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackage.Types.SpekeKeyProvider
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

instance Data.FromJSON MssEncryption where
  parseJSON =
    Data.withObject
      "MssEncryption"
      ( \x ->
          MssEncryption'
            Prelude.<$> (x Data..: "spekeKeyProvider")
      )

instance Prelude.Hashable MssEncryption where
  hashWithSalt _salt MssEncryption' {..} =
    _salt `Prelude.hashWithSalt` spekeKeyProvider

instance Prelude.NFData MssEncryption where
  rnf MssEncryption' {..} = Prelude.rnf spekeKeyProvider

instance Data.ToJSON MssEncryption where
  toJSON MssEncryption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("spekeKeyProvider" Data..= spekeKeyProvider)
          ]
      )
