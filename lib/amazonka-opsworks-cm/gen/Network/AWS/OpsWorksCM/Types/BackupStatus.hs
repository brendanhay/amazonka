{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.BackupStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.BackupStatus where

import Network.AWS.Prelude

data BackupStatus
  = BSDeleting
  | BSFailed
  | BSInProgress
  | BSOK
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

instance FromText BackupStatus where
  parser =
    takeLowerText >>= \case
      "deleting" -> pure BSDeleting
      "failed" -> pure BSFailed
      "in_progress" -> pure BSInProgress
      "ok" -> pure BSOK
      e ->
        fromTextError $
          "Failure parsing BackupStatus from value: '" <> e
            <> "'. Accepted values: deleting, failed, in_progress, ok"

instance ToText BackupStatus where
  toText = \case
    BSDeleting -> "DELETING"
    BSFailed -> "FAILED"
    BSInProgress -> "IN_PROGRESS"
    BSOK -> "OK"

instance Hashable BackupStatus

instance NFData BackupStatus

instance ToByteString BackupStatus

instance ToQuery BackupStatus

instance ToHeader BackupStatus

instance FromJSON BackupStatus where
  parseJSON = parseJSONText "BackupStatus"
