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
-- Module      : Amazonka.DirectoryService.Types.IpRouteInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.IpRouteInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.IpRouteStatusMsg
import qualified Amazonka.Prelude as Prelude

-- | Information about one or more IP address blocks.
--
-- /See:/ 'newIpRouteInfo' smart constructor.
data IpRouteInfo = IpRouteInfo'
  { -- | The status of the IP address block.
    ipRouteStatusMsg :: Prelude.Maybe IpRouteStatusMsg,
    -- | Identifier (ID) of the directory associated with the IP addresses.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | Description of the IpRouteInfo.
    description :: Prelude.Maybe Prelude.Text,
    -- | IP address block in the IpRoute.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | The date and time the address block was added to the directory.
    addedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The reason for the IpRouteStatusMsg.
    ipRouteStatusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpRouteInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipRouteStatusMsg', 'ipRouteInfo_ipRouteStatusMsg' - The status of the IP address block.
--
-- 'directoryId', 'ipRouteInfo_directoryId' - Identifier (ID) of the directory associated with the IP addresses.
--
-- 'description', 'ipRouteInfo_description' - Description of the IpRouteInfo.
--
-- 'cidrIp', 'ipRouteInfo_cidrIp' - IP address block in the IpRoute.
--
-- 'addedDateTime', 'ipRouteInfo_addedDateTime' - The date and time the address block was added to the directory.
--
-- 'ipRouteStatusReason', 'ipRouteInfo_ipRouteStatusReason' - The reason for the IpRouteStatusMsg.
newIpRouteInfo ::
  IpRouteInfo
newIpRouteInfo =
  IpRouteInfo'
    { ipRouteStatusMsg = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      description = Prelude.Nothing,
      cidrIp = Prelude.Nothing,
      addedDateTime = Prelude.Nothing,
      ipRouteStatusReason = Prelude.Nothing
    }

-- | The status of the IP address block.
ipRouteInfo_ipRouteStatusMsg :: Lens.Lens' IpRouteInfo (Prelude.Maybe IpRouteStatusMsg)
ipRouteInfo_ipRouteStatusMsg = Lens.lens (\IpRouteInfo' {ipRouteStatusMsg} -> ipRouteStatusMsg) (\s@IpRouteInfo' {} a -> s {ipRouteStatusMsg = a} :: IpRouteInfo)

-- | Identifier (ID) of the directory associated with the IP addresses.
ipRouteInfo_directoryId :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_directoryId = Lens.lens (\IpRouteInfo' {directoryId} -> directoryId) (\s@IpRouteInfo' {} a -> s {directoryId = a} :: IpRouteInfo)

-- | Description of the IpRouteInfo.
ipRouteInfo_description :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_description = Lens.lens (\IpRouteInfo' {description} -> description) (\s@IpRouteInfo' {} a -> s {description = a} :: IpRouteInfo)

-- | IP address block in the IpRoute.
ipRouteInfo_cidrIp :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_cidrIp = Lens.lens (\IpRouteInfo' {cidrIp} -> cidrIp) (\s@IpRouteInfo' {} a -> s {cidrIp = a} :: IpRouteInfo)

-- | The date and time the address block was added to the directory.
ipRouteInfo_addedDateTime :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.UTCTime)
ipRouteInfo_addedDateTime = Lens.lens (\IpRouteInfo' {addedDateTime} -> addedDateTime) (\s@IpRouteInfo' {} a -> s {addedDateTime = a} :: IpRouteInfo) Prelude.. Lens.mapping Data._Time

-- | The reason for the IpRouteStatusMsg.
ipRouteInfo_ipRouteStatusReason :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_ipRouteStatusReason = Lens.lens (\IpRouteInfo' {ipRouteStatusReason} -> ipRouteStatusReason) (\s@IpRouteInfo' {} a -> s {ipRouteStatusReason = a} :: IpRouteInfo)

instance Data.FromJSON IpRouteInfo where
  parseJSON =
    Data.withObject
      "IpRouteInfo"
      ( \x ->
          IpRouteInfo'
            Prelude.<$> (x Data..:? "IpRouteStatusMsg")
            Prelude.<*> (x Data..:? "DirectoryId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "CidrIp")
            Prelude.<*> (x Data..:? "AddedDateTime")
            Prelude.<*> (x Data..:? "IpRouteStatusReason")
      )

instance Prelude.Hashable IpRouteInfo where
  hashWithSalt _salt IpRouteInfo' {..} =
    _salt `Prelude.hashWithSalt` ipRouteStatusMsg
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` cidrIp
      `Prelude.hashWithSalt` addedDateTime
      `Prelude.hashWithSalt` ipRouteStatusReason

instance Prelude.NFData IpRouteInfo where
  rnf IpRouteInfo' {..} =
    Prelude.rnf ipRouteStatusMsg
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf cidrIp
      `Prelude.seq` Prelude.rnf addedDateTime
      `Prelude.seq` Prelude.rnf ipRouteStatusReason
