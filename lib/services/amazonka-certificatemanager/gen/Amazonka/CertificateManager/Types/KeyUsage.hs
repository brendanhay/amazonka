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
-- Module      : Amazonka.CertificateManager.Types.KeyUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManager.Types.KeyUsage where

import Amazonka.CertificateManager.Types.KeyUsageName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Key Usage X.509 v3 extension defines the purpose of the public key
-- contained in the certificate.
--
-- /See:/ 'newKeyUsage' smart constructor.
data KeyUsage = KeyUsage'
  { -- | A string value that contains a Key Usage extension name.
    name :: Prelude.Maybe KeyUsageName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KeyUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'keyUsage_name' - A string value that contains a Key Usage extension name.
newKeyUsage ::
  KeyUsage
newKeyUsage = KeyUsage' {name = Prelude.Nothing}

-- | A string value that contains a Key Usage extension name.
keyUsage_name :: Lens.Lens' KeyUsage (Prelude.Maybe KeyUsageName)
keyUsage_name = Lens.lens (\KeyUsage' {name} -> name) (\s@KeyUsage' {} a -> s {name = a} :: KeyUsage)

instance Data.FromJSON KeyUsage where
  parseJSON =
    Data.withObject
      "KeyUsage"
      (\x -> KeyUsage' Prelude.<$> (x Data..:? "Name"))

instance Prelude.Hashable KeyUsage where
  hashWithSalt _salt KeyUsage' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData KeyUsage where
  rnf KeyUsage' {..} = Prelude.rnf name
