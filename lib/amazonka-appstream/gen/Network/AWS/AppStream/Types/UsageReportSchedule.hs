{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UsageReportSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UsageReportSchedule where

import Network.AWS.Prelude

data UsageReportSchedule = Daily
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

instance FromText UsageReportSchedule where
  parser =
    takeLowerText >>= \case
      "daily" -> pure Daily
      e ->
        fromTextError $
          "Failure parsing UsageReportSchedule from value: '" <> e
            <> "'. Accepted values: daily"

instance ToText UsageReportSchedule where
  toText = \case
    Daily -> "DAILY"

instance Hashable UsageReportSchedule

instance NFData UsageReportSchedule

instance ToByteString UsageReportSchedule

instance ToQuery UsageReportSchedule

instance ToHeader UsageReportSchedule

instance FromJSON UsageReportSchedule where
  parseJSON = parseJSONText "UsageReportSchedule"
