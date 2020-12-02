{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UsageLimitLimitType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UsageLimitLimitType where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data UsageLimitLimitType
  = DataScanned
  | Time
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

instance FromText UsageLimitLimitType where
  parser =
    takeLowerText >>= \case
      "data-scanned" -> pure DataScanned
      "time" -> pure Time
      e ->
        fromTextError $
          "Failure parsing UsageLimitLimitType from value: '" <> e
            <> "'. Accepted values: data-scanned, time"

instance ToText UsageLimitLimitType where
  toText = \case
    DataScanned -> "data-scanned"
    Time -> "time"

instance Hashable UsageLimitLimitType

instance NFData UsageLimitLimitType

instance ToByteString UsageLimitLimitType

instance ToQuery UsageLimitLimitType

instance ToHeader UsageLimitLimitType

instance FromXML UsageLimitLimitType where
  parseXML = parseXMLText "UsageLimitLimitType"
