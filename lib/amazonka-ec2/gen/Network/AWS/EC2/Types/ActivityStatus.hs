{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ActivityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ActivityStatus where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ActivityStatus
  = ASError'
  | ASFulfilled
  | ASPendingFulfillment
  | ASPendingTermination
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

instance FromText ActivityStatus where
  parser =
    takeLowerText >>= \case
      "error" -> pure ASError'
      "fulfilled" -> pure ASFulfilled
      "pending_fulfillment" -> pure ASPendingFulfillment
      "pending_termination" -> pure ASPendingTermination
      e ->
        fromTextError $
          "Failure parsing ActivityStatus from value: '" <> e
            <> "'. Accepted values: error, fulfilled, pending_fulfillment, pending_termination"

instance ToText ActivityStatus where
  toText = \case
    ASError' -> "error"
    ASFulfilled -> "fulfilled"
    ASPendingFulfillment -> "pending_fulfillment"
    ASPendingTermination -> "pending_termination"

instance Hashable ActivityStatus

instance NFData ActivityStatus

instance ToByteString ActivityStatus

instance ToQuery ActivityStatus

instance ToHeader ActivityStatus

instance FromXML ActivityStatus where
  parseXML = parseXMLText "ActivityStatus"
