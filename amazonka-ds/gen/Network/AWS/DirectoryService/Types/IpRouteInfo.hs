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
-- Module      : Network.AWS.DirectoryService.Types.IpRouteInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.IpRouteInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.IpRouteStatusMsg
import qualified Network.AWS.Lens as Lens

-- | Information about one or more IP address blocks.
--
-- /See:/ 'newIpRouteInfo' smart constructor.
data IpRouteInfo = IpRouteInfo'
  { -- | IP address block in the IpRoute.
    cidrIp :: Core.Maybe Core.Text,
    -- | The status of the IP address block.
    ipRouteStatusMsg :: Core.Maybe IpRouteStatusMsg,
    -- | Identifier (ID) of the directory associated with the IP addresses.
    directoryId :: Core.Maybe Core.Text,
    -- | The date and time the address block was added to the directory.
    addedDateTime :: Core.Maybe Core.POSIX,
    -- | Description of the IpRouteInfo.
    description :: Core.Maybe Core.Text,
    -- | The reason for the IpRouteStatusMsg.
    ipRouteStatusReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IpRouteInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrIp', 'ipRouteInfo_cidrIp' - IP address block in the IpRoute.
--
-- 'ipRouteStatusMsg', 'ipRouteInfo_ipRouteStatusMsg' - The status of the IP address block.
--
-- 'directoryId', 'ipRouteInfo_directoryId' - Identifier (ID) of the directory associated with the IP addresses.
--
-- 'addedDateTime', 'ipRouteInfo_addedDateTime' - The date and time the address block was added to the directory.
--
-- 'description', 'ipRouteInfo_description' - Description of the IpRouteInfo.
--
-- 'ipRouteStatusReason', 'ipRouteInfo_ipRouteStatusReason' - The reason for the IpRouteStatusMsg.
newIpRouteInfo ::
  IpRouteInfo
newIpRouteInfo =
  IpRouteInfo'
    { cidrIp = Core.Nothing,
      ipRouteStatusMsg = Core.Nothing,
      directoryId = Core.Nothing,
      addedDateTime = Core.Nothing,
      description = Core.Nothing,
      ipRouteStatusReason = Core.Nothing
    }

-- | IP address block in the IpRoute.
ipRouteInfo_cidrIp :: Lens.Lens' IpRouteInfo (Core.Maybe Core.Text)
ipRouteInfo_cidrIp = Lens.lens (\IpRouteInfo' {cidrIp} -> cidrIp) (\s@IpRouteInfo' {} a -> s {cidrIp = a} :: IpRouteInfo)

-- | The status of the IP address block.
ipRouteInfo_ipRouteStatusMsg :: Lens.Lens' IpRouteInfo (Core.Maybe IpRouteStatusMsg)
ipRouteInfo_ipRouteStatusMsg = Lens.lens (\IpRouteInfo' {ipRouteStatusMsg} -> ipRouteStatusMsg) (\s@IpRouteInfo' {} a -> s {ipRouteStatusMsg = a} :: IpRouteInfo)

-- | Identifier (ID) of the directory associated with the IP addresses.
ipRouteInfo_directoryId :: Lens.Lens' IpRouteInfo (Core.Maybe Core.Text)
ipRouteInfo_directoryId = Lens.lens (\IpRouteInfo' {directoryId} -> directoryId) (\s@IpRouteInfo' {} a -> s {directoryId = a} :: IpRouteInfo)

-- | The date and time the address block was added to the directory.
ipRouteInfo_addedDateTime :: Lens.Lens' IpRouteInfo (Core.Maybe Core.UTCTime)
ipRouteInfo_addedDateTime = Lens.lens (\IpRouteInfo' {addedDateTime} -> addedDateTime) (\s@IpRouteInfo' {} a -> s {addedDateTime = a} :: IpRouteInfo) Core.. Lens.mapping Core._Time

-- | Description of the IpRouteInfo.
ipRouteInfo_description :: Lens.Lens' IpRouteInfo (Core.Maybe Core.Text)
ipRouteInfo_description = Lens.lens (\IpRouteInfo' {description} -> description) (\s@IpRouteInfo' {} a -> s {description = a} :: IpRouteInfo)

-- | The reason for the IpRouteStatusMsg.
ipRouteInfo_ipRouteStatusReason :: Lens.Lens' IpRouteInfo (Core.Maybe Core.Text)
ipRouteInfo_ipRouteStatusReason = Lens.lens (\IpRouteInfo' {ipRouteStatusReason} -> ipRouteStatusReason) (\s@IpRouteInfo' {} a -> s {ipRouteStatusReason = a} :: IpRouteInfo)

instance Core.FromJSON IpRouteInfo where
  parseJSON =
    Core.withObject
      "IpRouteInfo"
      ( \x ->
          IpRouteInfo'
            Core.<$> (x Core..:? "CidrIp")
            Core.<*> (x Core..:? "IpRouteStatusMsg")
            Core.<*> (x Core..:? "DirectoryId")
            Core.<*> (x Core..:? "AddedDateTime")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "IpRouteStatusReason")
      )

instance Core.Hashable IpRouteInfo

instance Core.NFData IpRouteInfo
