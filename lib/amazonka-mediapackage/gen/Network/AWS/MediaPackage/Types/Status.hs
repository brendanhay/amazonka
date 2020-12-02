{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.Status
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.Status where

import Network.AWS.Prelude

data Status
  = Failed
  | InProgress
  | Succeeded
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

instance FromText Status where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing Status from value: '" <> e
            <> "'. Accepted values: failed, in_progress, succeeded"

instance ToText Status where
  toText = \case
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"
    Succeeded -> "SUCCEEDED"

instance Hashable Status

instance NFData Status

instance ToByteString Status

instance ToQuery Status

instance ToHeader Status

instance FromJSON Status where
  parseJSON = parseJSONText "Status"
