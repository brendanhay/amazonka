{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AlertTargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AlertTargetType where

import Network.AWS.Prelude

-- | The type of alert target: one of "SNS".
data AlertTargetType = SNS
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

instance FromText AlertTargetType where
  parser =
    takeLowerText >>= \case
      "sns" -> pure SNS
      e ->
        fromTextError $
          "Failure parsing AlertTargetType from value: '" <> e
            <> "'. Accepted values: sns"

instance ToText AlertTargetType where
  toText = \case
    SNS -> "SNS"

instance Hashable AlertTargetType

instance NFData AlertTargetType

instance ToByteString AlertTargetType

instance ToQuery AlertTargetType

instance ToHeader AlertTargetType

instance ToJSON AlertTargetType where
  toJSON = toJSONText

instance FromJSON AlertTargetType where
  parseJSON = parseJSONText "AlertTargetType"
