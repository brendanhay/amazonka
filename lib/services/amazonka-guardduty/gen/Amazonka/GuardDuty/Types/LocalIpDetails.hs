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
-- Module      : Amazonka.GuardDuty.Types.LocalIpDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.LocalIpDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the local IP address of the connection.
--
-- /See:/ 'newLocalIpDetails' smart constructor.
data LocalIpDetails = LocalIpDetails'
  { -- | The IPv4 local address of the connection.
    ipAddressV4 :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocalIpDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressV4', 'localIpDetails_ipAddressV4' - The IPv4 local address of the connection.
newLocalIpDetails ::
  LocalIpDetails
newLocalIpDetails =
  LocalIpDetails' {ipAddressV4 = Prelude.Nothing}

-- | The IPv4 local address of the connection.
localIpDetails_ipAddressV4 :: Lens.Lens' LocalIpDetails (Prelude.Maybe Prelude.Text)
localIpDetails_ipAddressV4 = Lens.lens (\LocalIpDetails' {ipAddressV4} -> ipAddressV4) (\s@LocalIpDetails' {} a -> s {ipAddressV4 = a} :: LocalIpDetails)

instance Core.FromJSON LocalIpDetails where
  parseJSON =
    Core.withObject
      "LocalIpDetails"
      ( \x ->
          LocalIpDetails'
            Prelude.<$> (x Core..:? "ipAddressV4")
      )

instance Prelude.Hashable LocalIpDetails where
  hashWithSalt _salt LocalIpDetails' {..} =
    _salt `Prelude.hashWithSalt` ipAddressV4

instance Prelude.NFData LocalIpDetails where
  rnf LocalIpDetails' {..} = Prelude.rnf ipAddressV4
