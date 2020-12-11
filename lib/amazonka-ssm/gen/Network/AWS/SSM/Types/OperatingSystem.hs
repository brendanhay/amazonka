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
        AmazonLinux,
        AmazonLinux2,
        Centos,
        Debian,
        OracleLinux,
        RedhatEnterpriseLinux,
        Suse,
        Ubuntu,
        Windows
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

pattern AmazonLinux :: OperatingSystem
pattern AmazonLinux = OperatingSystem' "AMAZON_LINUX"

pattern AmazonLinux2 :: OperatingSystem
pattern AmazonLinux2 = OperatingSystem' "AMAZON_LINUX_2"

pattern Centos :: OperatingSystem
pattern Centos = OperatingSystem' "CENTOS"

pattern Debian :: OperatingSystem
pattern Debian = OperatingSystem' "DEBIAN"

pattern OracleLinux :: OperatingSystem
pattern OracleLinux = OperatingSystem' "ORACLE_LINUX"

pattern RedhatEnterpriseLinux :: OperatingSystem
pattern RedhatEnterpriseLinux = OperatingSystem' "REDHAT_ENTERPRISE_LINUX"

pattern Suse :: OperatingSystem
pattern Suse = OperatingSystem' "SUSE"

pattern Ubuntu :: OperatingSystem
pattern Ubuntu = OperatingSystem' "UBUNTU"

pattern Windows :: OperatingSystem
pattern Windows = OperatingSystem' "WINDOWS"

{-# COMPLETE
  AmazonLinux,
  AmazonLinux2,
  Centos,
  Debian,
  OracleLinux,
  RedhatEnterpriseLinux,
  Suse,
  Ubuntu,
  Windows,
  OperatingSystem'
  #-}
