{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OperatingSystem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OperatingSystem
  ( OperatingSystem
      ( OperatingSystem',
        OSWindows,
        OSAmazonLinux,
        OSAmazonLinux2,
        OSUbuntu,
        OSRedhatEnterpriseLinux,
        OSSuse,
        OSCentos,
        OSOracleLinux,
        OSDebian
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OperatingSystem = OperatingSystem' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern OSWindows :: OperatingSystem
pattern OSWindows = OperatingSystem' "WINDOWS"

pattern OSAmazonLinux :: OperatingSystem
pattern OSAmazonLinux = OperatingSystem' "AMAZON_LINUX"

pattern OSAmazonLinux2 :: OperatingSystem
pattern OSAmazonLinux2 = OperatingSystem' "AMAZON_LINUX_2"

pattern OSUbuntu :: OperatingSystem
pattern OSUbuntu = OperatingSystem' "UBUNTU"

pattern OSRedhatEnterpriseLinux :: OperatingSystem
pattern OSRedhatEnterpriseLinux = OperatingSystem' "REDHAT_ENTERPRISE_LINUX"

pattern OSSuse :: OperatingSystem
pattern OSSuse = OperatingSystem' "SUSE"

pattern OSCentos :: OperatingSystem
pattern OSCentos = OperatingSystem' "CENTOS"

pattern OSOracleLinux :: OperatingSystem
pattern OSOracleLinux = OperatingSystem' "ORACLE_LINUX"

pattern OSDebian :: OperatingSystem
pattern OSDebian = OperatingSystem' "DEBIAN"

{-# COMPLETE
  OSWindows,
  OSAmazonLinux,
  OSAmazonLinux2,
  OSUbuntu,
  OSRedhatEnterpriseLinux,
  OSSuse,
  OSCentos,
  OSOracleLinux,
  OSDebian,
  OperatingSystem'
  #-}
