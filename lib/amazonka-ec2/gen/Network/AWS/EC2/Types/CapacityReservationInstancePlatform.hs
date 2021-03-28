{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationInstancePlatform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CapacityReservationInstancePlatform
  ( CapacityReservationInstancePlatform
    ( CapacityReservationInstancePlatform'
    , CapacityReservationInstancePlatformLinuxUnix
    , CapacityReservationInstancePlatformRedHatEnterpriseLinux
    , CapacityReservationInstancePlatformSuseLinux
    , CapacityReservationInstancePlatformWindows
    , CapacityReservationInstancePlatformWindowsWithSqlServer
    , CapacityReservationInstancePlatformWindowsWithSqlServerEnterprise
    , CapacityReservationInstancePlatformWindowsWithSqlServerStandard
    , CapacityReservationInstancePlatformWindowsWithSqlServerWeb
    , CapacityReservationInstancePlatformLinuxWithSqlServerStandard
    , CapacityReservationInstancePlatformLinuxWithSqlServerWeb
    , CapacityReservationInstancePlatformLinuxWithSqlServerEnterprise
    , fromCapacityReservationInstancePlatform
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype CapacityReservationInstancePlatform = CapacityReservationInstancePlatform'{fromCapacityReservationInstancePlatform
                                                                                   :: Core.Text}
                                                deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                Core.Show, Core.Generic)
                                                deriving newtype (Core.IsString, Core.Hashable,
                                                                  Core.NFData, Core.ToJSONKey,
                                                                  Core.FromJSONKey, Core.ToJSON,
                                                                  Core.FromJSON, Core.ToXML,
                                                                  Core.FromXML, Core.ToText,
                                                                  Core.FromText, Core.ToByteString,
                                                                  Core.ToQuery, Core.ToHeader)

pattern CapacityReservationInstancePlatformLinuxUnix :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformLinuxUnix = CapacityReservationInstancePlatform' "Linux/UNIX"

pattern CapacityReservationInstancePlatformRedHatEnterpriseLinux :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformRedHatEnterpriseLinux = CapacityReservationInstancePlatform' "Red Hat Enterprise Linux"

pattern CapacityReservationInstancePlatformSuseLinux :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformSuseLinux = CapacityReservationInstancePlatform' "SUSE Linux"

pattern CapacityReservationInstancePlatformWindows :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformWindows = CapacityReservationInstancePlatform' "Windows"

pattern CapacityReservationInstancePlatformWindowsWithSqlServer :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformWindowsWithSqlServer = CapacityReservationInstancePlatform' "Windows with SQL Server"

pattern CapacityReservationInstancePlatformWindowsWithSqlServerEnterprise :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformWindowsWithSqlServerEnterprise = CapacityReservationInstancePlatform' "Windows with SQL Server Enterprise"

pattern CapacityReservationInstancePlatformWindowsWithSqlServerStandard :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformWindowsWithSqlServerStandard = CapacityReservationInstancePlatform' "Windows with SQL Server Standard"

pattern CapacityReservationInstancePlatformWindowsWithSqlServerWeb :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformWindowsWithSqlServerWeb = CapacityReservationInstancePlatform' "Windows with SQL Server Web"

pattern CapacityReservationInstancePlatformLinuxWithSqlServerStandard :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformLinuxWithSqlServerStandard = CapacityReservationInstancePlatform' "Linux with SQL Server Standard"

pattern CapacityReservationInstancePlatformLinuxWithSqlServerWeb :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformLinuxWithSqlServerWeb = CapacityReservationInstancePlatform' "Linux with SQL Server Web"

pattern CapacityReservationInstancePlatformLinuxWithSqlServerEnterprise :: CapacityReservationInstancePlatform
pattern CapacityReservationInstancePlatformLinuxWithSqlServerEnterprise = CapacityReservationInstancePlatform' "Linux with SQL Server Enterprise"

{-# COMPLETE 
  CapacityReservationInstancePlatformLinuxUnix,

  CapacityReservationInstancePlatformRedHatEnterpriseLinux,

  CapacityReservationInstancePlatformSuseLinux,

  CapacityReservationInstancePlatformWindows,

  CapacityReservationInstancePlatformWindowsWithSqlServer,

  CapacityReservationInstancePlatformWindowsWithSqlServerEnterprise,

  CapacityReservationInstancePlatformWindowsWithSqlServerStandard,

  CapacityReservationInstancePlatformWindowsWithSqlServerWeb,

  CapacityReservationInstancePlatformLinuxWithSqlServerStandard,

  CapacityReservationInstancePlatformLinuxWithSqlServerWeb,

  CapacityReservationInstancePlatformLinuxWithSqlServerEnterprise,
  CapacityReservationInstancePlatform'
  #-}
