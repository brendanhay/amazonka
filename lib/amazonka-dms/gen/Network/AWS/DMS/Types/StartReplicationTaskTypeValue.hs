{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.StartReplicationTaskTypeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.StartReplicationTaskTypeValue where

import Network.AWS.Prelude

data StartReplicationTaskTypeValue
  = ReloadTarget
  | ResumeProcessing
  | StartReplication
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

instance FromText StartReplicationTaskTypeValue where
  parser =
    takeLowerText >>= \case
      "reload-target" -> pure ReloadTarget
      "resume-processing" -> pure ResumeProcessing
      "start-replication" -> pure StartReplication
      e ->
        fromTextError $
          "Failure parsing StartReplicationTaskTypeValue from value: '" <> e
            <> "'. Accepted values: reload-target, resume-processing, start-replication"

instance ToText StartReplicationTaskTypeValue where
  toText = \case
    ReloadTarget -> "reload-target"
    ResumeProcessing -> "resume-processing"
    StartReplication -> "start-replication"

instance Hashable StartReplicationTaskTypeValue

instance NFData StartReplicationTaskTypeValue

instance ToByteString StartReplicationTaskTypeValue

instance ToQuery StartReplicationTaskTypeValue

instance ToHeader StartReplicationTaskTypeValue

instance ToJSON StartReplicationTaskTypeValue where
  toJSON = toJSONText
