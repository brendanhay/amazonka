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
-- Module      : Network.AWS.CertificateManager.Types.KeyUsage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.KeyUsage where

import Network.AWS.CertificateManager.Types.KeyUsageName
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The Key Usage X.509 v3 extension defines the purpose of the public key
-- contained in the certificate.
--
-- /See:/ 'newKeyUsage' smart constructor.
data KeyUsage = KeyUsage'
  { -- | A string value that contains a Key Usage extension name.
    name :: Core.Maybe KeyUsageName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newKeyUsage = KeyUsage' {name = Core.Nothing}

-- | A string value that contains a Key Usage extension name.
keyUsage_name :: Lens.Lens' KeyUsage (Core.Maybe KeyUsageName)
keyUsage_name = Lens.lens (\KeyUsage' {name} -> name) (\s@KeyUsage' {} a -> s {name = a} :: KeyUsage)

instance Core.FromJSON KeyUsage where
  parseJSON =
    Core.withObject
      "KeyUsage"
      (\x -> KeyUsage' Core.<$> (x Core..:? "Name"))

instance Core.Hashable KeyUsage

instance Core.NFData KeyUsage
