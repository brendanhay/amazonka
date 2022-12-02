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
-- Module      : Amazonka.SESV2.Types.DedicatedIp
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DedicatedIp where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.WarmupStatus

-- | Contains information about a dedicated IP address that is associated
-- with your Amazon SES account.
--
-- To learn more about requesting dedicated IP addresses, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/dedicated-ip-case.html Requesting and Relinquishing Dedicated IP Addresses>
-- in the /Amazon SES Developer Guide/.
--
-- /See:/ 'newDedicatedIp' smart constructor.
data DedicatedIp = DedicatedIp'
  { -- | The name of the dedicated IP pool that the IP address is associated
    -- with.
    poolName :: Prelude.Maybe Prelude.Text,
    -- | An IPv4 address.
    ip :: Prelude.Text,
    -- | The warm-up status of a dedicated IP address. The status can have one of
    -- the following values:
    --
    -- -   @IN_PROGRESS@ – The IP address isn\'t ready to use because the
    --     dedicated IP warm-up process is ongoing.
    --
    -- -   @DONE@ – The dedicated IP warm-up process is complete, and the IP
    --     address is ready to use.
    warmupStatus :: WarmupStatus,
    -- | Indicates how complete the dedicated IP warm-up process is. When this
    -- value equals 1, the address has completed the warm-up process and is
    -- ready for use.
    warmupPercentage :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DedicatedIp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolName', 'dedicatedIp_poolName' - The name of the dedicated IP pool that the IP address is associated
-- with.
--
-- 'ip', 'dedicatedIp_ip' - An IPv4 address.
--
-- 'warmupStatus', 'dedicatedIp_warmupStatus' - The warm-up status of a dedicated IP address. The status can have one of
-- the following values:
--
-- -   @IN_PROGRESS@ – The IP address isn\'t ready to use because the
--     dedicated IP warm-up process is ongoing.
--
-- -   @DONE@ – The dedicated IP warm-up process is complete, and the IP
--     address is ready to use.
--
-- 'warmupPercentage', 'dedicatedIp_warmupPercentage' - Indicates how complete the dedicated IP warm-up process is. When this
-- value equals 1, the address has completed the warm-up process and is
-- ready for use.
newDedicatedIp ::
  -- | 'ip'
  Prelude.Text ->
  -- | 'warmupStatus'
  WarmupStatus ->
  -- | 'warmupPercentage'
  Prelude.Int ->
  DedicatedIp
newDedicatedIp pIp_ pWarmupStatus_ pWarmupPercentage_ =
  DedicatedIp'
    { poolName = Prelude.Nothing,
      ip = pIp_,
      warmupStatus = pWarmupStatus_,
      warmupPercentage = pWarmupPercentage_
    }

-- | The name of the dedicated IP pool that the IP address is associated
-- with.
dedicatedIp_poolName :: Lens.Lens' DedicatedIp (Prelude.Maybe Prelude.Text)
dedicatedIp_poolName = Lens.lens (\DedicatedIp' {poolName} -> poolName) (\s@DedicatedIp' {} a -> s {poolName = a} :: DedicatedIp)

-- | An IPv4 address.
dedicatedIp_ip :: Lens.Lens' DedicatedIp Prelude.Text
dedicatedIp_ip = Lens.lens (\DedicatedIp' {ip} -> ip) (\s@DedicatedIp' {} a -> s {ip = a} :: DedicatedIp)

-- | The warm-up status of a dedicated IP address. The status can have one of
-- the following values:
--
-- -   @IN_PROGRESS@ – The IP address isn\'t ready to use because the
--     dedicated IP warm-up process is ongoing.
--
-- -   @DONE@ – The dedicated IP warm-up process is complete, and the IP
--     address is ready to use.
dedicatedIp_warmupStatus :: Lens.Lens' DedicatedIp WarmupStatus
dedicatedIp_warmupStatus = Lens.lens (\DedicatedIp' {warmupStatus} -> warmupStatus) (\s@DedicatedIp' {} a -> s {warmupStatus = a} :: DedicatedIp)

-- | Indicates how complete the dedicated IP warm-up process is. When this
-- value equals 1, the address has completed the warm-up process and is
-- ready for use.
dedicatedIp_warmupPercentage :: Lens.Lens' DedicatedIp Prelude.Int
dedicatedIp_warmupPercentage = Lens.lens (\DedicatedIp' {warmupPercentage} -> warmupPercentage) (\s@DedicatedIp' {} a -> s {warmupPercentage = a} :: DedicatedIp)

instance Data.FromJSON DedicatedIp where
  parseJSON =
    Data.withObject
      "DedicatedIp"
      ( \x ->
          DedicatedIp'
            Prelude.<$> (x Data..:? "PoolName")
            Prelude.<*> (x Data..: "Ip")
            Prelude.<*> (x Data..: "WarmupStatus")
            Prelude.<*> (x Data..: "WarmupPercentage")
      )

instance Prelude.Hashable DedicatedIp where
  hashWithSalt _salt DedicatedIp' {..} =
    _salt `Prelude.hashWithSalt` poolName
      `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` warmupStatus
      `Prelude.hashWithSalt` warmupPercentage

instance Prelude.NFData DedicatedIp where
  rnf DedicatedIp' {..} =
    Prelude.rnf poolName
      `Prelude.seq` Prelude.rnf ip
      `Prelude.seq` Prelude.rnf warmupStatus
      `Prelude.seq` Prelude.rnf warmupPercentage
