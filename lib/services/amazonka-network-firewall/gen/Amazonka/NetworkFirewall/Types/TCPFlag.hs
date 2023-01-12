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
-- Module      : Amazonka.NetworkFirewall.Types.TCPFlag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.TCPFlag
  ( TCPFlag
      ( ..,
        TCPFlag_ACK,
        TCPFlag_CWR,
        TCPFlag_ECE,
        TCPFlag_FIN,
        TCPFlag_PSH,
        TCPFlag_RST,
        TCPFlag_SYN,
        TCPFlag_URG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TCPFlag = TCPFlag' {fromTCPFlag :: Data.Text}
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

pattern TCPFlag_ACK :: TCPFlag
pattern TCPFlag_ACK = TCPFlag' "ACK"

pattern TCPFlag_CWR :: TCPFlag
pattern TCPFlag_CWR = TCPFlag' "CWR"

pattern TCPFlag_ECE :: TCPFlag
pattern TCPFlag_ECE = TCPFlag' "ECE"

pattern TCPFlag_FIN :: TCPFlag
pattern TCPFlag_FIN = TCPFlag' "FIN"

pattern TCPFlag_PSH :: TCPFlag
pattern TCPFlag_PSH = TCPFlag' "PSH"

pattern TCPFlag_RST :: TCPFlag
pattern TCPFlag_RST = TCPFlag' "RST"

pattern TCPFlag_SYN :: TCPFlag
pattern TCPFlag_SYN = TCPFlag' "SYN"

pattern TCPFlag_URG :: TCPFlag
pattern TCPFlag_URG = TCPFlag' "URG"

{-# COMPLETE
  TCPFlag_ACK,
  TCPFlag_CWR,
  TCPFlag_ECE,
  TCPFlag_FIN,
  TCPFlag_PSH,
  TCPFlag_RST,
  TCPFlag_SYN,
  TCPFlag_URG,
  TCPFlag'
  #-}
