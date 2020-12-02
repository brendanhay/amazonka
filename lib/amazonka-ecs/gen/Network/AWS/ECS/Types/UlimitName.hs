{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.UlimitName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.UlimitName where

import Network.AWS.Prelude

data UlimitName
  = CPU
  | Core
  | Data
  | Fsize
  | Locks
  | Memlock
  | Msgqueue
  | Nice
  | Nofile
  | Nproc
  | Rss
  | Rtprio
  | Rttime
  | Sigpending
  | Stack
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText UlimitName where
  parser =
    takeLowerText >>= \case
      "cpu" -> pure CPU
      "core" -> pure Core
      "data" -> pure Data
      "fsize" -> pure Fsize
      "locks" -> pure Locks
      "memlock" -> pure Memlock
      "msgqueue" -> pure Msgqueue
      "nice" -> pure Nice
      "nofile" -> pure Nofile
      "nproc" -> pure Nproc
      "rss" -> pure Rss
      "rtprio" -> pure Rtprio
      "rttime" -> pure Rttime
      "sigpending" -> pure Sigpending
      "stack" -> pure Stack
      e ->
        fromTextError $
          "Failure parsing UlimitName from value: '" <> e
            <> "'. Accepted values: cpu, core, data, fsize, locks, memlock, msgqueue, nice, nofile, nproc, rss, rtprio, rttime, sigpending, stack"

instance ToText UlimitName where
  toText = \case
    CPU -> "cpu"
    Core -> "core"
    Data -> "data"
    Fsize -> "fsize"
    Locks -> "locks"
    Memlock -> "memlock"
    Msgqueue -> "msgqueue"
    Nice -> "nice"
    Nofile -> "nofile"
    Nproc -> "nproc"
    Rss -> "rss"
    Rtprio -> "rtprio"
    Rttime -> "rttime"
    Sigpending -> "sigpending"
    Stack -> "stack"

instance Hashable UlimitName

instance NFData UlimitName

instance ToByteString UlimitName

instance ToQuery UlimitName

instance ToHeader UlimitName

instance ToJSON UlimitName where
  toJSON = toJSONText

instance FromJSON UlimitName where
  parseJSON = parseJSONText "UlimitName"
