{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.DsnAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.DsnAction where

import Network.AWS.Prelude

data DsnAction
  = DADelayed
  | DADelivered
  | DAExpanded
  | DAFailed
  | DARelayed
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

instance FromText DsnAction where
  parser =
    takeLowerText >>= \case
      "delayed" -> pure DADelayed
      "delivered" -> pure DADelivered
      "expanded" -> pure DAExpanded
      "failed" -> pure DAFailed
      "relayed" -> pure DARelayed
      e ->
        fromTextError $
          "Failure parsing DsnAction from value: '" <> e
            <> "'. Accepted values: delayed, delivered, expanded, failed, relayed"

instance ToText DsnAction where
  toText = \case
    DADelayed -> "delayed"
    DADelivered -> "delivered"
    DAExpanded -> "expanded"
    DAFailed -> "failed"
    DARelayed -> "relayed"

instance Hashable DsnAction

instance NFData DsnAction

instance ToByteString DsnAction

instance ToQuery DsnAction

instance ToHeader DsnAction
