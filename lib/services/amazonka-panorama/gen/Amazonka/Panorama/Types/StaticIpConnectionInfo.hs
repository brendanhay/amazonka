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
-- Module      : Amazonka.Panorama.Types.StaticIpConnectionInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.StaticIpConnectionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A static IP configuration.
--
-- /See:/ 'newStaticIpConnectionInfo' smart constructor.
data StaticIpConnectionInfo = StaticIpConnectionInfo'
  { -- | The connection\'s default gateway.
    defaultGateway :: Prelude.Text,
    -- | The connection\'s DNS address.
    dns :: [Prelude.Text],
    -- | The connection\'s IP address.
    ipAddress :: Prelude.Text,
    -- | The connection\'s DNS mask.
    mask :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StaticIpConnectionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultGateway', 'staticIpConnectionInfo_defaultGateway' - The connection\'s default gateway.
--
-- 'dns', 'staticIpConnectionInfo_dns' - The connection\'s DNS address.
--
-- 'ipAddress', 'staticIpConnectionInfo_ipAddress' - The connection\'s IP address.
--
-- 'mask', 'staticIpConnectionInfo_mask' - The connection\'s DNS mask.
newStaticIpConnectionInfo ::
  -- | 'defaultGateway'
  Prelude.Text ->
  -- | 'ipAddress'
  Prelude.Text ->
  -- | 'mask'
  Prelude.Text ->
  StaticIpConnectionInfo
newStaticIpConnectionInfo
  pDefaultGateway_
  pIpAddress_
  pMask_ =
    StaticIpConnectionInfo'
      { defaultGateway =
          pDefaultGateway_,
        dns = Prelude.mempty,
        ipAddress = pIpAddress_,
        mask = pMask_
      }

-- | The connection\'s default gateway.
staticIpConnectionInfo_defaultGateway :: Lens.Lens' StaticIpConnectionInfo Prelude.Text
staticIpConnectionInfo_defaultGateway = Lens.lens (\StaticIpConnectionInfo' {defaultGateway} -> defaultGateway) (\s@StaticIpConnectionInfo' {} a -> s {defaultGateway = a} :: StaticIpConnectionInfo)

-- | The connection\'s DNS address.
staticIpConnectionInfo_dns :: Lens.Lens' StaticIpConnectionInfo [Prelude.Text]
staticIpConnectionInfo_dns = Lens.lens (\StaticIpConnectionInfo' {dns} -> dns) (\s@StaticIpConnectionInfo' {} a -> s {dns = a} :: StaticIpConnectionInfo) Prelude.. Lens.coerced

-- | The connection\'s IP address.
staticIpConnectionInfo_ipAddress :: Lens.Lens' StaticIpConnectionInfo Prelude.Text
staticIpConnectionInfo_ipAddress = Lens.lens (\StaticIpConnectionInfo' {ipAddress} -> ipAddress) (\s@StaticIpConnectionInfo' {} a -> s {ipAddress = a} :: StaticIpConnectionInfo)

-- | The connection\'s DNS mask.
staticIpConnectionInfo_mask :: Lens.Lens' StaticIpConnectionInfo Prelude.Text
staticIpConnectionInfo_mask = Lens.lens (\StaticIpConnectionInfo' {mask} -> mask) (\s@StaticIpConnectionInfo' {} a -> s {mask = a} :: StaticIpConnectionInfo)

instance Data.FromJSON StaticIpConnectionInfo where
  parseJSON =
    Data.withObject
      "StaticIpConnectionInfo"
      ( \x ->
          StaticIpConnectionInfo'
            Prelude.<$> (x Data..: "DefaultGateway")
            Prelude.<*> (x Data..:? "Dns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "IpAddress")
            Prelude.<*> (x Data..: "Mask")
      )

instance Prelude.Hashable StaticIpConnectionInfo where
  hashWithSalt _salt StaticIpConnectionInfo' {..} =
    _salt
      `Prelude.hashWithSalt` defaultGateway
      `Prelude.hashWithSalt` dns
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` mask

instance Prelude.NFData StaticIpConnectionInfo where
  rnf StaticIpConnectionInfo' {..} =
    Prelude.rnf defaultGateway
      `Prelude.seq` Prelude.rnf dns
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf mask

instance Data.ToJSON StaticIpConnectionInfo where
  toJSON StaticIpConnectionInfo' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DefaultGateway" Data..= defaultGateway),
            Prelude.Just ("Dns" Data..= dns),
            Prelude.Just ("IpAddress" Data..= ipAddress),
            Prelude.Just ("Mask" Data..= mask)
          ]
      )
