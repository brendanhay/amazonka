{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.NetworkFirewall.Types.StatefulRuleProtocol
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatefulRuleProtocol
  ( StatefulRuleProtocol
      ( ..,
        StatefulRuleProtocol_DCERPC,
        StatefulRuleProtocol_DHCP,
        StatefulRuleProtocol_DNS,
        StatefulRuleProtocol_FTP,
        StatefulRuleProtocol_HTTP,
        StatefulRuleProtocol_ICMP,
        StatefulRuleProtocol_IKEV2,
        StatefulRuleProtocol_IMAP,
        StatefulRuleProtocol_IP,
        StatefulRuleProtocol_KRB5,
        StatefulRuleProtocol_MSN,
        StatefulRuleProtocol_NTP,
        StatefulRuleProtocol_SMB,
        StatefulRuleProtocol_SMTP,
        StatefulRuleProtocol_SSH,
        StatefulRuleProtocol_TCP,
        StatefulRuleProtocol_TFTP,
        StatefulRuleProtocol_TLS,
        StatefulRuleProtocol_UDP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StatefulRuleProtocol = StatefulRuleProtocol'
  { fromStatefulRuleProtocol ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern StatefulRuleProtocol_DCERPC :: StatefulRuleProtocol
pattern StatefulRuleProtocol_DCERPC = StatefulRuleProtocol' "DCERPC"

pattern StatefulRuleProtocol_DHCP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_DHCP = StatefulRuleProtocol' "DHCP"

pattern StatefulRuleProtocol_DNS :: StatefulRuleProtocol
pattern StatefulRuleProtocol_DNS = StatefulRuleProtocol' "DNS"

pattern StatefulRuleProtocol_FTP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_FTP = StatefulRuleProtocol' "FTP"

pattern StatefulRuleProtocol_HTTP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_HTTP = StatefulRuleProtocol' "HTTP"

pattern StatefulRuleProtocol_ICMP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_ICMP = StatefulRuleProtocol' "ICMP"

pattern StatefulRuleProtocol_IKEV2 :: StatefulRuleProtocol
pattern StatefulRuleProtocol_IKEV2 = StatefulRuleProtocol' "IKEV2"

pattern StatefulRuleProtocol_IMAP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_IMAP = StatefulRuleProtocol' "IMAP"

pattern StatefulRuleProtocol_IP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_IP = StatefulRuleProtocol' "IP"

pattern StatefulRuleProtocol_KRB5 :: StatefulRuleProtocol
pattern StatefulRuleProtocol_KRB5 = StatefulRuleProtocol' "KRB5"

pattern StatefulRuleProtocol_MSN :: StatefulRuleProtocol
pattern StatefulRuleProtocol_MSN = StatefulRuleProtocol' "MSN"

pattern StatefulRuleProtocol_NTP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_NTP = StatefulRuleProtocol' "NTP"

pattern StatefulRuleProtocol_SMB :: StatefulRuleProtocol
pattern StatefulRuleProtocol_SMB = StatefulRuleProtocol' "SMB"

pattern StatefulRuleProtocol_SMTP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_SMTP = StatefulRuleProtocol' "SMTP"

pattern StatefulRuleProtocol_SSH :: StatefulRuleProtocol
pattern StatefulRuleProtocol_SSH = StatefulRuleProtocol' "SSH"

pattern StatefulRuleProtocol_TCP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_TCP = StatefulRuleProtocol' "TCP"

pattern StatefulRuleProtocol_TFTP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_TFTP = StatefulRuleProtocol' "TFTP"

pattern StatefulRuleProtocol_TLS :: StatefulRuleProtocol
pattern StatefulRuleProtocol_TLS = StatefulRuleProtocol' "TLS"

pattern StatefulRuleProtocol_UDP :: StatefulRuleProtocol
pattern StatefulRuleProtocol_UDP = StatefulRuleProtocol' "UDP"

{-# COMPLETE
  StatefulRuleProtocol_DCERPC,
  StatefulRuleProtocol_DHCP,
  StatefulRuleProtocol_DNS,
  StatefulRuleProtocol_FTP,
  StatefulRuleProtocol_HTTP,
  StatefulRuleProtocol_ICMP,
  StatefulRuleProtocol_IKEV2,
  StatefulRuleProtocol_IMAP,
  StatefulRuleProtocol_IP,
  StatefulRuleProtocol_KRB5,
  StatefulRuleProtocol_MSN,
  StatefulRuleProtocol_NTP,
  StatefulRuleProtocol_SMB,
  StatefulRuleProtocol_SMTP,
  StatefulRuleProtocol_SSH,
  StatefulRuleProtocol_TCP,
  StatefulRuleProtocol_TFTP,
  StatefulRuleProtocol_TLS,
  StatefulRuleProtocol_UDP,
  StatefulRuleProtocol'
  #-}
