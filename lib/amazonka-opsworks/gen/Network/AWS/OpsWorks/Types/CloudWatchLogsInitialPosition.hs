{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.CloudWatchLogsInitialPosition where

import Network.AWS.Prelude

-- | Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. It's only used if there is no state persisted for that log stream.
data CloudWatchLogsInitialPosition
  = EndOfFile
  | StartOfFile
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

instance FromText CloudWatchLogsInitialPosition where
  parser =
    takeLowerText >>= \case
      "end_of_file" -> pure EndOfFile
      "start_of_file" -> pure StartOfFile
      e ->
        fromTextError $
          "Failure parsing CloudWatchLogsInitialPosition from value: '" <> e
            <> "'. Accepted values: end_of_file, start_of_file"

instance ToText CloudWatchLogsInitialPosition where
  toText = \case
    EndOfFile -> "end_of_file"
    StartOfFile -> "start_of_file"

instance Hashable CloudWatchLogsInitialPosition

instance NFData CloudWatchLogsInitialPosition

instance ToByteString CloudWatchLogsInitialPosition

instance ToQuery CloudWatchLogsInitialPosition

instance ToHeader CloudWatchLogsInitialPosition

instance ToJSON CloudWatchLogsInitialPosition where
  toJSON = toJSONText

instance FromJSON CloudWatchLogsInitialPosition where
  parseJSON = parseJSONText "CloudWatchLogsInitialPosition"
