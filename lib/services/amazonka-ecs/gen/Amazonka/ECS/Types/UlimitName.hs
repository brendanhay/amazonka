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
-- Module      : Amazonka.ECS.Types.UlimitName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.UlimitName
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UlimitName = UlimitName'
  { fromUlimitName ::
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
