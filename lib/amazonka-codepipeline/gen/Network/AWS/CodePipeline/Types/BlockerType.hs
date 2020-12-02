{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.BlockerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.BlockerType where

import Network.AWS.Prelude

data BlockerType = Schedule
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

instance FromText BlockerType where
  parser =
    takeLowerText >>= \case
      "schedule" -> pure Schedule
      e ->
        fromTextError $
          "Failure parsing BlockerType from value: '" <> e
            <> "'. Accepted values: schedule"

instance ToText BlockerType where
  toText = \case
    Schedule -> "Schedule"

instance Hashable BlockerType

instance NFData BlockerType

instance ToByteString BlockerType

instance ToQuery BlockerType

instance ToHeader BlockerType

instance ToJSON BlockerType where
  toJSON = toJSONText

instance FromJSON BlockerType where
  parseJSON = parseJSONText "BlockerType"
