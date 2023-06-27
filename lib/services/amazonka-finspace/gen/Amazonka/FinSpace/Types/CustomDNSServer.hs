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
-- Module      : Amazonka.FinSpace.Types.CustomDNSServer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Types.CustomDNSServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of DNS server name and server IP. This is used to set up Route-53
-- outbound resolvers.
--
-- /See:/ 'newCustomDNSServer' smart constructor.
data CustomDNSServer = CustomDNSServer'
  { -- | The name of the DNS server.
    customDNSServerName :: Prelude.Text,
    -- | The IP address of the DNS server.
    customDNSServerIP :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomDNSServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customDNSServerName', 'customDNSServer_customDNSServerName' - The name of the DNS server.
--
-- 'customDNSServerIP', 'customDNSServer_customDNSServerIP' - The IP address of the DNS server.
newCustomDNSServer ::
  -- | 'customDNSServerName'
  Prelude.Text ->
  -- | 'customDNSServerIP'
  Prelude.Text ->
  CustomDNSServer
newCustomDNSServer
  pCustomDNSServerName_
  pCustomDNSServerIP_ =
    CustomDNSServer'
      { customDNSServerName =
          pCustomDNSServerName_,
        customDNSServerIP = pCustomDNSServerIP_
      }

-- | The name of the DNS server.
customDNSServer_customDNSServerName :: Lens.Lens' CustomDNSServer Prelude.Text
customDNSServer_customDNSServerName = Lens.lens (\CustomDNSServer' {customDNSServerName} -> customDNSServerName) (\s@CustomDNSServer' {} a -> s {customDNSServerName = a} :: CustomDNSServer)

-- | The IP address of the DNS server.
customDNSServer_customDNSServerIP :: Lens.Lens' CustomDNSServer Prelude.Text
customDNSServer_customDNSServerIP = Lens.lens (\CustomDNSServer' {customDNSServerIP} -> customDNSServerIP) (\s@CustomDNSServer' {} a -> s {customDNSServerIP = a} :: CustomDNSServer)

instance Data.FromJSON CustomDNSServer where
  parseJSON =
    Data.withObject
      "CustomDNSServer"
      ( \x ->
          CustomDNSServer'
            Prelude.<$> (x Data..: "customDNSServerName")
            Prelude.<*> (x Data..: "customDNSServerIP")
      )

instance Prelude.Hashable CustomDNSServer where
  hashWithSalt _salt CustomDNSServer' {..} =
    _salt
      `Prelude.hashWithSalt` customDNSServerName
      `Prelude.hashWithSalt` customDNSServerIP

instance Prelude.NFData CustomDNSServer where
  rnf CustomDNSServer' {..} =
    Prelude.rnf customDNSServerName
      `Prelude.seq` Prelude.rnf customDNSServerIP

instance Data.ToJSON CustomDNSServer where
  toJSON CustomDNSServer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("customDNSServerName" Data..= customDNSServerName),
            Prelude.Just
              ("customDNSServerIP" Data..= customDNSServerIP)
          ]
      )
