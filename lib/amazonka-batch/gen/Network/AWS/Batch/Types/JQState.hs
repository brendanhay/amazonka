{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JQState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JQState where

import Network.AWS.Prelude

data JQState
  = JQSDisabled
  | JQSEnabled
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

instance FromText JQState where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure JQSDisabled
      "enabled" -> pure JQSEnabled
      e ->
        fromTextError $
          "Failure parsing JQState from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText JQState where
  toText = \case
    JQSDisabled -> "DISABLED"
    JQSEnabled -> "ENABLED"

instance Hashable JQState

instance NFData JQState

instance ToByteString JQState

instance ToQuery JQState

instance ToHeader JQState

instance ToJSON JQState where
  toJSON = toJSONText

instance FromJSON JQState where
  parseJSON = parseJSONText "JQState"
