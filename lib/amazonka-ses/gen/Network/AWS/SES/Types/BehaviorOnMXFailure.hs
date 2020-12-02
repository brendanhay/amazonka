{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BehaviorOnMXFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BehaviorOnMXFailure where

import Network.AWS.Prelude

data BehaviorOnMXFailure
  = RejectMessage
  | UseDefaultValue
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

instance FromText BehaviorOnMXFailure where
  parser =
    takeLowerText >>= \case
      "rejectmessage" -> pure RejectMessage
      "usedefaultvalue" -> pure UseDefaultValue
      e ->
        fromTextError $
          "Failure parsing BehaviorOnMXFailure from value: '" <> e
            <> "'. Accepted values: rejectmessage, usedefaultvalue"

instance ToText BehaviorOnMXFailure where
  toText = \case
    RejectMessage -> "RejectMessage"
    UseDefaultValue -> "UseDefaultValue"

instance Hashable BehaviorOnMXFailure

instance NFData BehaviorOnMXFailure

instance ToByteString BehaviorOnMXFailure

instance ToQuery BehaviorOnMXFailure

instance ToHeader BehaviorOnMXFailure

instance FromXML BehaviorOnMXFailure where
  parseXML = parseXMLText "BehaviorOnMXFailure"
