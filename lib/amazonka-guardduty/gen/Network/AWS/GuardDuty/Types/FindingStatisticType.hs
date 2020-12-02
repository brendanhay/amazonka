{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FindingStatisticType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FindingStatisticType where

import Network.AWS.Prelude

data FindingStatisticType = CountBySeverity
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

instance FromText FindingStatisticType where
  parser =
    takeLowerText >>= \case
      "count_by_severity" -> pure CountBySeverity
      e ->
        fromTextError $
          "Failure parsing FindingStatisticType from value: '" <> e
            <> "'. Accepted values: count_by_severity"

instance ToText FindingStatisticType where
  toText = \case
    CountBySeverity -> "COUNT_BY_SEVERITY"

instance Hashable FindingStatisticType

instance NFData FindingStatisticType

instance ToByteString FindingStatisticType

instance ToQuery FindingStatisticType

instance ToHeader FindingStatisticType

instance ToJSON FindingStatisticType where
  toJSON = toJSONText
