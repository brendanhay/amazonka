{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.TaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.TaskStatus where

import Network.AWS.Prelude

data TaskStatus
  = Failed
  | False'
  | Finished
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

instance FromText TaskStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure Failed
      "false" -> pure False'
      "finished" -> pure Finished
      e ->
        fromTextError $
          "Failure parsing TaskStatus from value: '" <> e
            <> "'. Accepted values: failed, false, finished"

instance ToText TaskStatus where
  toText = \case
    Failed -> "FAILED"
    False' -> "FALSE"
    Finished -> "FINISHED"

instance Hashable TaskStatus

instance NFData TaskStatus

instance ToByteString TaskStatus

instance ToQuery TaskStatus

instance ToHeader TaskStatus

instance ToJSON TaskStatus where
  toJSON = toJSONText
