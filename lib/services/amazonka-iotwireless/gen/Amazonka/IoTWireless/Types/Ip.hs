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
-- Module      : Amazonka.IoTWireless.Types.Ip
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.Ip where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | IP address used for resolving device location.
--
-- /See:/ 'newIp' smart constructor.
data Ip = Ip'
  { -- | IP address information.
    ipAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Ip' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'ip_ipAddress' - IP address information.
newIp ::
  -- | 'ipAddress'
  Prelude.Text ->
  Ip
newIp pIpAddress_ = Ip' {ipAddress = pIpAddress_}

-- | IP address information.
ip_ipAddress :: Lens.Lens' Ip Prelude.Text
ip_ipAddress = Lens.lens (\Ip' {ipAddress} -> ipAddress) (\s@Ip' {} a -> s {ipAddress = a} :: Ip)

instance Prelude.Hashable Ip where
  hashWithSalt _salt Ip' {..} =
    _salt `Prelude.hashWithSalt` ipAddress

instance Prelude.NFData Ip where
  rnf Ip' {..} = Prelude.rnf ipAddress

instance Data.ToJSON Ip where
  toJSON Ip' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("IpAddress" Data..= ipAddress)]
      )
