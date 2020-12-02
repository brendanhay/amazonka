{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.CalendarState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.CalendarState where

import Network.AWS.Prelude

data CalendarState
  = CSClosed
  | CSOpen
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

instance FromText CalendarState where
  parser =
    takeLowerText >>= \case
      "closed" -> pure CSClosed
      "open" -> pure CSOpen
      e ->
        fromTextError $
          "Failure parsing CalendarState from value: '" <> e
            <> "'. Accepted values: closed, open"

instance ToText CalendarState where
  toText = \case
    CSClosed -> "CLOSED"
    CSOpen -> "OPEN"

instance Hashable CalendarState

instance NFData CalendarState

instance ToByteString CalendarState

instance ToQuery CalendarState

instance ToHeader CalendarState

instance FromJSON CalendarState where
  parseJSON = parseJSONText "CalendarState"
