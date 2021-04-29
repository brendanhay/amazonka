{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.ProtocolEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.ProtocolEnum
  ( ProtocolEnum
      ( ..,
        ProtocolEnum_GENEVE,
        ProtocolEnum_HTTP,
        ProtocolEnum_HTTPS,
        ProtocolEnum_TCP,
        ProtocolEnum_TCP_UDP,
        ProtocolEnum_TLS,
        ProtocolEnum_UDP
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ProtocolEnum = ProtocolEnum'
  { fromProtocolEnum ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ProtocolEnum_GENEVE :: ProtocolEnum
pattern ProtocolEnum_GENEVE = ProtocolEnum' "GENEVE"

pattern ProtocolEnum_HTTP :: ProtocolEnum
pattern ProtocolEnum_HTTP = ProtocolEnum' "HTTP"

pattern ProtocolEnum_HTTPS :: ProtocolEnum
pattern ProtocolEnum_HTTPS = ProtocolEnum' "HTTPS"

pattern ProtocolEnum_TCP :: ProtocolEnum
pattern ProtocolEnum_TCP = ProtocolEnum' "TCP"

pattern ProtocolEnum_TCP_UDP :: ProtocolEnum
pattern ProtocolEnum_TCP_UDP = ProtocolEnum' "TCP_UDP"

pattern ProtocolEnum_TLS :: ProtocolEnum
pattern ProtocolEnum_TLS = ProtocolEnum' "TLS"

pattern ProtocolEnum_UDP :: ProtocolEnum
pattern ProtocolEnum_UDP = ProtocolEnum' "UDP"

{-# COMPLETE
  ProtocolEnum_GENEVE,
  ProtocolEnum_HTTP,
  ProtocolEnum_HTTPS,
  ProtocolEnum_TCP,
  ProtocolEnum_TCP_UDP,
  ProtocolEnum_TLS,
  ProtocolEnum_UDP,
  ProtocolEnum'
  #-}
