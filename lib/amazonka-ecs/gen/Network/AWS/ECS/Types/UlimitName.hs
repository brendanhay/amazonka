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
        CPU,
        Core,
        Data,
        Fsize,
        Locks,
        Memlock,
        Msgqueue,
        Nice,
        Nofile,
        Nproc,
        Rss,
        Rtprio,
        Rttime,
        Sigpending,
        Stack
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UlimitName = UlimitName' Lude.Text
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

pattern CPU :: UlimitName
pattern CPU = UlimitName' "cpu"

pattern Core :: UlimitName
pattern Core = UlimitName' "core"

pattern Data :: UlimitName
pattern Data = UlimitName' "data"

pattern Fsize :: UlimitName
pattern Fsize = UlimitName' "fsize"

pattern Locks :: UlimitName
pattern Locks = UlimitName' "locks"

pattern Memlock :: UlimitName
pattern Memlock = UlimitName' "memlock"

pattern Msgqueue :: UlimitName
pattern Msgqueue = UlimitName' "msgqueue"

pattern Nice :: UlimitName
pattern Nice = UlimitName' "nice"

pattern Nofile :: UlimitName
pattern Nofile = UlimitName' "nofile"

pattern Nproc :: UlimitName
pattern Nproc = UlimitName' "nproc"

pattern Rss :: UlimitName
pattern Rss = UlimitName' "rss"

pattern Rtprio :: UlimitName
pattern Rtprio = UlimitName' "rtprio"

pattern Rttime :: UlimitName
pattern Rttime = UlimitName' "rttime"

pattern Sigpending :: UlimitName
pattern Sigpending = UlimitName' "sigpending"

pattern Stack :: UlimitName
pattern Stack = UlimitName' "stack"

{-# COMPLETE
  CPU,
  Core,
  Data,
  Fsize,
  Locks,
  Memlock,
  Msgqueue,
  Nice,
  Nofile,
  Nproc,
  Rss,
  Rtprio,
  Rttime,
  Sigpending,
  Stack,
  UlimitName'
  #-}
