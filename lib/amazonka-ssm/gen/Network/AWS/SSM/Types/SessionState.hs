{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SessionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SessionState where

import Network.AWS.Prelude

data SessionState
  = Active
  | History
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

instance FromText SessionState where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "history" -> pure History
      e ->
        fromTextError $
          "Failure parsing SessionState from value: '" <> e
            <> "'. Accepted values: active, history"

instance ToText SessionState where
  toText = \case
    Active -> "Active"
    History -> "History"

instance Hashable SessionState

instance NFData SessionState

instance ToByteString SessionState

instance ToQuery SessionState

instance ToHeader SessionState

instance ToJSON SessionState where
  toJSON = toJSONText
