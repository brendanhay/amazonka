{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ConversionTaskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ConversionTaskState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data ConversionTaskState
  = CTSActive
  | CTSCancelled
  | CTSCancelling
  | CTSCompleted
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

instance FromText ConversionTaskState where
  parser =
    takeLowerText >>= \case
      "active" -> pure CTSActive
      "cancelled" -> pure CTSCancelled
      "cancelling" -> pure CTSCancelling
      "completed" -> pure CTSCompleted
      e ->
        fromTextError $
          "Failure parsing ConversionTaskState from value: '" <> e
            <> "'. Accepted values: active, cancelled, cancelling, completed"

instance ToText ConversionTaskState where
  toText = \case
    CTSActive -> "active"
    CTSCancelled -> "cancelled"
    CTSCancelling -> "cancelling"
    CTSCompleted -> "completed"

instance Hashable ConversionTaskState

instance NFData ConversionTaskState

instance ToByteString ConversionTaskState

instance ToQuery ConversionTaskState

instance ToHeader ConversionTaskState

instance FromXML ConversionTaskState where
  parseXML = parseXMLText "ConversionTaskState"
