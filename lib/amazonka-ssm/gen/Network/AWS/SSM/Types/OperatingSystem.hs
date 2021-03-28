{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OperatingSystem
  ( OperatingSystem
    ( OperatingSystem'
    , OperatingSystemWindows
    , OperatingSystemAmazonLinux
    , OperatingSystemAmazonLinux2
    , OperatingSystemUbuntu
    , OperatingSystemRedhatEnterpriseLinux
    , OperatingSystemSuse
    , OperatingSystemCentos
    , OperatingSystemOracleLinux
    , OperatingSystemDebian
    , fromOperatingSystem
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype OperatingSystem = OperatingSystem'{fromOperatingSystem ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern OperatingSystemWindows :: OperatingSystem
pattern OperatingSystemWindows = OperatingSystem' "WINDOWS"

pattern OperatingSystemAmazonLinux :: OperatingSystem
pattern OperatingSystemAmazonLinux = OperatingSystem' "AMAZON_LINUX"

pattern OperatingSystemAmazonLinux2 :: OperatingSystem
pattern OperatingSystemAmazonLinux2 = OperatingSystem' "AMAZON_LINUX_2"

pattern OperatingSystemUbuntu :: OperatingSystem
pattern OperatingSystemUbuntu = OperatingSystem' "UBUNTU"

pattern OperatingSystemRedhatEnterpriseLinux :: OperatingSystem
pattern OperatingSystemRedhatEnterpriseLinux = OperatingSystem' "REDHAT_ENTERPRISE_LINUX"

pattern OperatingSystemSuse :: OperatingSystem
pattern OperatingSystemSuse = OperatingSystem' "SUSE"

pattern OperatingSystemCentos :: OperatingSystem
pattern OperatingSystemCentos = OperatingSystem' "CENTOS"

pattern OperatingSystemOracleLinux :: OperatingSystem
pattern OperatingSystemOracleLinux = OperatingSystem' "ORACLE_LINUX"

pattern OperatingSystemDebian :: OperatingSystem
pattern OperatingSystemDebian = OperatingSystem' "DEBIAN"

{-# COMPLETE 
  OperatingSystemWindows,

  OperatingSystemAmazonLinux,

  OperatingSystemAmazonLinux2,

  OperatingSystemUbuntu,

  OperatingSystemRedhatEnterpriseLinux,

  OperatingSystemSuse,

  OperatingSystemCentos,

  OperatingSystemOracleLinux,

  OperatingSystemDebian,
  OperatingSystem'
  #-}
