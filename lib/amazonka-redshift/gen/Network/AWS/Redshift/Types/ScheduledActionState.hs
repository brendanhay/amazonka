{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledActionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledActionState where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ScheduledActionState
  = SASActive
  | SASDisabled
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

instance FromText ScheduledActionState where
  parser =
    takeLowerText >>= \case
      "active" -> pure SASActive
      "disabled" -> pure SASDisabled
      e ->
        fromTextError $
          "Failure parsing ScheduledActionState from value: '" <> e
            <> "'. Accepted values: active, disabled"

instance ToText ScheduledActionState where
  toText = \case
    SASActive -> "ACTIVE"
    SASDisabled -> "DISABLED"

instance Hashable ScheduledActionState

instance NFData ScheduledActionState

instance ToByteString ScheduledActionState

instance ToQuery ScheduledActionState

instance ToHeader ScheduledActionState

instance FromXML ScheduledActionState where
  parseXML = parseXMLText "ScheduledActionState"
