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
-- Module      : Network.AWS.Panorama.Types.StaticIpConnectionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Panorama.Types.StaticIpConnectionInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A static IP configuration.
--
-- /See:/ 'newStaticIpConnectionInfo' smart constructor.
data StaticIpConnectionInfo = StaticIpConnectionInfo'
  { -- | The connection\'s IP address.
    ipAddress :: Prelude.Text,
    -- | The connection\'s DNS mask.
    mask :: Prelude.Text,
    -- | The connection\'s DNS address.
    dns :: [Prelude.Text],
    -- | The connection\'s default gateway.
    defaultGateway :: Prelude.Text
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
-- 'ipAddress', 'staticIpConnectionInfo_ipAddress' - The connection\'s IP address.
--
-- 'mask', 'staticIpConnectionInfo_mask' - The connection\'s DNS mask.
--
-- 'dns', 'staticIpConnectionInfo_dns' - The connection\'s DNS address.
--
-- 'defaultGateway', 'staticIpConnectionInfo_defaultGateway' - The connection\'s default gateway.
newStaticIpConnectionInfo ::
  -- | 'ipAddress'
  Prelude.Text ->
  -- | 'mask'
  Prelude.Text ->
  -- | 'defaultGateway'
  Prelude.Text ->
  StaticIpConnectionInfo
newStaticIpConnectionInfo
  pIpAddress_
  pMask_
  pDefaultGateway_ =
    StaticIpConnectionInfo'
      { ipAddress = pIpAddress_,
        mask = pMask_,
        dns = Prelude.mempty,
        defaultGateway = pDefaultGateway_
      }

-- | The connection\'s IP address.
staticIpConnectionInfo_ipAddress :: Lens.Lens' StaticIpConnectionInfo Prelude.Text
staticIpConnectionInfo_ipAddress = Lens.lens (\StaticIpConnectionInfo' {ipAddress} -> ipAddress) (\s@StaticIpConnectionInfo' {} a -> s {ipAddress = a} :: StaticIpConnectionInfo)

-- | The connection\'s DNS mask.
staticIpConnectionInfo_mask :: Lens.Lens' StaticIpConnectionInfo Prelude.Text
staticIpConnectionInfo_mask = Lens.lens (\StaticIpConnectionInfo' {mask} -> mask) (\s@StaticIpConnectionInfo' {} a -> s {mask = a} :: StaticIpConnectionInfo)

-- | The connection\'s DNS address.
staticIpConnectionInfo_dns :: Lens.Lens' StaticIpConnectionInfo [Prelude.Text]
staticIpConnectionInfo_dns = Lens.lens (\StaticIpConnectionInfo' {dns} -> dns) (\s@StaticIpConnectionInfo' {} a -> s {dns = a} :: StaticIpConnectionInfo) Prelude.. Lens.coerced

-- | The connection\'s default gateway.
staticIpConnectionInfo_defaultGateway :: Lens.Lens' StaticIpConnectionInfo Prelude.Text
staticIpConnectionInfo_defaultGateway = Lens.lens (\StaticIpConnectionInfo' {defaultGateway} -> defaultGateway) (\s@StaticIpConnectionInfo' {} a -> s {defaultGateway = a} :: StaticIpConnectionInfo)

instance Core.FromJSON StaticIpConnectionInfo where
  parseJSON =
    Core.withObject
      "StaticIpConnectionInfo"
      ( \x ->
          StaticIpConnectionInfo'
            Prelude.<$> (x Core..: "IpAddress")
            Prelude.<*> (x Core..: "Mask")
            Prelude.<*> (x Core..:? "Dns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "DefaultGateway")
      )

instance Prelude.Hashable StaticIpConnectionInfo

instance Prelude.NFData StaticIpConnectionInfo

instance Core.ToJSON StaticIpConnectionInfo where
  toJSON StaticIpConnectionInfo' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("IpAddress" Core..= ipAddress),
            Prelude.Just ("Mask" Core..= mask),
            Prelude.Just ("Dns" Core..= dns),
            Prelude.Just
              ("DefaultGateway" Core..= defaultGateway)
          ]
      )
