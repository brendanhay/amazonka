{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectoryService.Types.IpRouteStatusMsg
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about one or more IP address blocks.
--
-- /See:/ 'newIpRouteInfo' smart constructor.
data IpRouteInfo = IpRouteInfo'
  { -- | IP address block in the IpRoute.
    cidrIp :: Prelude.Maybe Prelude.Text,
    -- | The status of the IP address block.
    ipRouteStatusMsg :: Prelude.Maybe IpRouteStatusMsg,
    -- | Identifier (ID) of the directory associated with the IP addresses.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The date and time the address block was added to the directory.
    addedDateTime :: Prelude.Maybe Prelude.POSIX,
    -- | Description of the IpRouteInfo.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reason for the IpRouteStatusMsg.
    ipRouteStatusReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { cidrIp = Prelude.Nothing,
      ipRouteStatusMsg = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      addedDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      ipRouteStatusReason = Prelude.Nothing
    }

-- | IP address block in the IpRoute.
ipRouteInfo_cidrIp :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_cidrIp = Lens.lens (\IpRouteInfo' {cidrIp} -> cidrIp) (\s@IpRouteInfo' {} a -> s {cidrIp = a} :: IpRouteInfo)

-- | The status of the IP address block.
ipRouteInfo_ipRouteStatusMsg :: Lens.Lens' IpRouteInfo (Prelude.Maybe IpRouteStatusMsg)
ipRouteInfo_ipRouteStatusMsg = Lens.lens (\IpRouteInfo' {ipRouteStatusMsg} -> ipRouteStatusMsg) (\s@IpRouteInfo' {} a -> s {ipRouteStatusMsg = a} :: IpRouteInfo)

-- | Identifier (ID) of the directory associated with the IP addresses.
ipRouteInfo_directoryId :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_directoryId = Lens.lens (\IpRouteInfo' {directoryId} -> directoryId) (\s@IpRouteInfo' {} a -> s {directoryId = a} :: IpRouteInfo)

-- | The date and time the address block was added to the directory.
ipRouteInfo_addedDateTime :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.UTCTime)
ipRouteInfo_addedDateTime = Lens.lens (\IpRouteInfo' {addedDateTime} -> addedDateTime) (\s@IpRouteInfo' {} a -> s {addedDateTime = a} :: IpRouteInfo) Prelude.. Lens.mapping Prelude._Time

-- | Description of the IpRouteInfo.
ipRouteInfo_description :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_description = Lens.lens (\IpRouteInfo' {description} -> description) (\s@IpRouteInfo' {} a -> s {description = a} :: IpRouteInfo)

-- | The reason for the IpRouteStatusMsg.
ipRouteInfo_ipRouteStatusReason :: Lens.Lens' IpRouteInfo (Prelude.Maybe Prelude.Text)
ipRouteInfo_ipRouteStatusReason = Lens.lens (\IpRouteInfo' {ipRouteStatusReason} -> ipRouteStatusReason) (\s@IpRouteInfo' {} a -> s {ipRouteStatusReason = a} :: IpRouteInfo)

instance Prelude.FromJSON IpRouteInfo where
  parseJSON =
    Prelude.withObject
      "IpRouteInfo"
      ( \x ->
          IpRouteInfo'
            Prelude.<$> (x Prelude..:? "CidrIp")
            Prelude.<*> (x Prelude..:? "IpRouteStatusMsg")
            Prelude.<*> (x Prelude..:? "DirectoryId")
            Prelude.<*> (x Prelude..:? "AddedDateTime")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "IpRouteStatusReason")
      )

instance Prelude.Hashable IpRouteInfo

instance Prelude.NFData IpRouteInfo
