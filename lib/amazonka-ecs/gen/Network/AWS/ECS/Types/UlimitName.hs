{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.UlimitName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.UlimitName
  ( UlimitName
      ( UlimitName',
        UlimitNameCore,
        UlimitNameCpu,
        UlimitNameData,
        UlimitNameFsize,
        UlimitNameLocks,
        UlimitNameMemlock,
        UlimitNameMsgqueue,
        UlimitNameNice,
        UlimitNameNofile,
        UlimitNameNproc,
        UlimitNameRss,
        UlimitNameRtprio,
        UlimitNameRttime,
        UlimitNameSigpending,
        UlimitNameStack,
        fromUlimitName
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype UlimitName = UlimitName' {fromUlimitName :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern UlimitNameCore :: UlimitName
pattern UlimitNameCore = UlimitName' "core"

pattern UlimitNameCpu :: UlimitName
pattern UlimitNameCpu = UlimitName' "cpu"

pattern UlimitNameData :: UlimitName
pattern UlimitNameData = UlimitName' "data"

pattern UlimitNameFsize :: UlimitName
pattern UlimitNameFsize = UlimitName' "fsize"

pattern UlimitNameLocks :: UlimitName
pattern UlimitNameLocks = UlimitName' "locks"

pattern UlimitNameMemlock :: UlimitName
pattern UlimitNameMemlock = UlimitName' "memlock"

pattern UlimitNameMsgqueue :: UlimitName
pattern UlimitNameMsgqueue = UlimitName' "msgqueue"

pattern UlimitNameNice :: UlimitName
pattern UlimitNameNice = UlimitName' "nice"

pattern UlimitNameNofile :: UlimitName
pattern UlimitNameNofile = UlimitName' "nofile"

pattern UlimitNameNproc :: UlimitName
pattern UlimitNameNproc = UlimitName' "nproc"

pattern UlimitNameRss :: UlimitName
pattern UlimitNameRss = UlimitName' "rss"

pattern UlimitNameRtprio :: UlimitName
pattern UlimitNameRtprio = UlimitName' "rtprio"

pattern UlimitNameRttime :: UlimitName
pattern UlimitNameRttime = UlimitName' "rttime"

pattern UlimitNameSigpending :: UlimitName
pattern UlimitNameSigpending = UlimitName' "sigpending"

pattern UlimitNameStack :: UlimitName
pattern UlimitNameStack = UlimitName' "stack"

{-# COMPLETE
  UlimitNameCore,
  UlimitNameCpu,
  UlimitNameData,
  UlimitNameFsize,
  UlimitNameLocks,
  UlimitNameMemlock,
  UlimitNameMsgqueue,
  UlimitNameNice,
  UlimitNameNofile,
  UlimitNameNproc,
  UlimitNameRss,
  UlimitNameRtprio,
  UlimitNameRttime,
  UlimitNameSigpending,
  UlimitNameStack,
  UlimitName'
  #-}
