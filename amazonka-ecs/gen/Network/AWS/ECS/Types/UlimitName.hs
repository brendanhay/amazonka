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
-- Module      : Network.AWS.ECS.Types.UlimitName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.UlimitName
  ( UlimitName
      ( ..,
        UlimitName_Core,
        UlimitName_Cpu,
        UlimitName_Data,
        UlimitName_Fsize,
        UlimitName_Locks,
        UlimitName_Memlock,
        UlimitName_Msgqueue,
        UlimitName_Nice,
        UlimitName_Nofile,
        UlimitName_Nproc,
        UlimitName_Rss,
        UlimitName_Rtprio,
        UlimitName_Rttime,
        UlimitName_Sigpending,
        UlimitName_Stack
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype UlimitName = UlimitName'
  { fromUlimitName ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern UlimitName_Core :: UlimitName
pattern UlimitName_Core = UlimitName' "core"

pattern UlimitName_Cpu :: UlimitName
pattern UlimitName_Cpu = UlimitName' "cpu"

pattern UlimitName_Data :: UlimitName
pattern UlimitName_Data = UlimitName' "data"

pattern UlimitName_Fsize :: UlimitName
pattern UlimitName_Fsize = UlimitName' "fsize"

pattern UlimitName_Locks :: UlimitName
pattern UlimitName_Locks = UlimitName' "locks"

pattern UlimitName_Memlock :: UlimitName
pattern UlimitName_Memlock = UlimitName' "memlock"

pattern UlimitName_Msgqueue :: UlimitName
pattern UlimitName_Msgqueue = UlimitName' "msgqueue"

pattern UlimitName_Nice :: UlimitName
pattern UlimitName_Nice = UlimitName' "nice"

pattern UlimitName_Nofile :: UlimitName
pattern UlimitName_Nofile = UlimitName' "nofile"

pattern UlimitName_Nproc :: UlimitName
pattern UlimitName_Nproc = UlimitName' "nproc"

pattern UlimitName_Rss :: UlimitName
pattern UlimitName_Rss = UlimitName' "rss"

pattern UlimitName_Rtprio :: UlimitName
pattern UlimitName_Rtprio = UlimitName' "rtprio"

pattern UlimitName_Rttime :: UlimitName
pattern UlimitName_Rttime = UlimitName' "rttime"

pattern UlimitName_Sigpending :: UlimitName
pattern UlimitName_Sigpending = UlimitName' "sigpending"

pattern UlimitName_Stack :: UlimitName
pattern UlimitName_Stack = UlimitName' "stack"

{-# COMPLETE
  UlimitName_Core,
  UlimitName_Cpu,
  UlimitName_Data,
  UlimitName_Fsize,
  UlimitName_Locks,
  UlimitName_Memlock,
  UlimitName_Msgqueue,
  UlimitName_Nice,
  UlimitName_Nofile,
  UlimitName_Nproc,
  UlimitName_Rss,
  UlimitName_Rtprio,
  UlimitName_Rttime,
  UlimitName_Sigpending,
  UlimitName_Stack,
  UlimitName'
  #-}
