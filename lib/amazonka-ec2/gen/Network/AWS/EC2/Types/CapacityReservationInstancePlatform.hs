{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationInstancePlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationInstancePlatform where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data CapacityReservationInstancePlatform
  = CRIPLinuxUnix
  | CRIPLinuxWithSqlServerEnterprise
  | CRIPLinuxWithSqlServerStandard
  | CRIPLinuxWithSqlServerWeb
  | CRIPRedHatEnterpriseLinux
  | CRIPSuseLinux
  | CRIPWindows
  | CRIPWindowsWithSqlServer
  | CRIPWindowsWithSqlServerEnterprise
  | CRIPWindowsWithSqlServerStandard
  | CRIPWindowsWithSqlServerWeb
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText CapacityReservationInstancePlatform where
  parser =
    takeLowerText >>= \case
      "linux/unix" -> pure CRIPLinuxUnix
      "linux with sql server enterprise" -> pure CRIPLinuxWithSqlServerEnterprise
      "linux with sql server standard" -> pure CRIPLinuxWithSqlServerStandard
      "linux with sql server web" -> pure CRIPLinuxWithSqlServerWeb
      "red hat enterprise linux" -> pure CRIPRedHatEnterpriseLinux
      "suse linux" -> pure CRIPSuseLinux
      "windows" -> pure CRIPWindows
      "windows with sql server" -> pure CRIPWindowsWithSqlServer
      "windows with sql server enterprise" -> pure CRIPWindowsWithSqlServerEnterprise
      "windows with sql server standard" -> pure CRIPWindowsWithSqlServerStandard
      "windows with sql server web" -> pure CRIPWindowsWithSqlServerWeb
      e ->
        fromTextError $
          "Failure parsing CapacityReservationInstancePlatform from value: '" <> e
            <> "'. Accepted values: linux/unix, linux with sql server enterprise, linux with sql server standard, linux with sql server web, red hat enterprise linux, suse linux, windows, windows with sql server, windows with sql server enterprise, windows with sql server standard, windows with sql server web"

instance ToText CapacityReservationInstancePlatform where
  toText = \case
    CRIPLinuxUnix -> "Linux/UNIX"
    CRIPLinuxWithSqlServerEnterprise -> "Linux with SQL Server Enterprise"
    CRIPLinuxWithSqlServerStandard -> "Linux with SQL Server Standard"
    CRIPLinuxWithSqlServerWeb -> "Linux with SQL Server Web"
    CRIPRedHatEnterpriseLinux -> "Red Hat Enterprise Linux"
    CRIPSuseLinux -> "SUSE Linux"
    CRIPWindows -> "Windows"
    CRIPWindowsWithSqlServer -> "Windows with SQL Server"
    CRIPWindowsWithSqlServerEnterprise -> "Windows with SQL Server Enterprise"
    CRIPWindowsWithSqlServerStandard -> "Windows with SQL Server Standard"
    CRIPWindowsWithSqlServerWeb -> "Windows with SQL Server Web"

instance Hashable CapacityReservationInstancePlatform

instance NFData CapacityReservationInstancePlatform

instance ToByteString CapacityReservationInstancePlatform

instance ToQuery CapacityReservationInstancePlatform

instance ToHeader CapacityReservationInstancePlatform

instance FromXML CapacityReservationInstancePlatform where
  parseXML = parseXMLText "CapacityReservationInstancePlatform"
