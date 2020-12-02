{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UsageLimitPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UsageLimitPeriod where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data UsageLimitPeriod
  = Daily
  | Monthly
  | Weekly
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

instance FromText UsageLimitPeriod where
  parser =
    takeLowerText >>= \case
      "daily" -> pure Daily
      "monthly" -> pure Monthly
      "weekly" -> pure Weekly
      e ->
        fromTextError $
          "Failure parsing UsageLimitPeriod from value: '" <> e
            <> "'. Accepted values: daily, monthly, weekly"

instance ToText UsageLimitPeriod where
  toText = \case
    Daily -> "daily"
    Monthly -> "monthly"
    Weekly -> "weekly"

instance Hashable UsageLimitPeriod

instance NFData UsageLimitPeriod

instance ToByteString UsageLimitPeriod

instance ToQuery UsageLimitPeriod

instance ToHeader UsageLimitPeriod

instance FromXML UsageLimitPeriod where
  parseXML = parseXMLText "UsageLimitPeriod"
