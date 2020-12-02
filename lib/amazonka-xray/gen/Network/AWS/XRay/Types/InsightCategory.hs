{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightCategory where

import Network.AWS.Prelude

data InsightCategory = Fault
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

instance FromText InsightCategory where
  parser =
    takeLowerText >>= \case
      "fault" -> pure Fault
      e ->
        fromTextError $
          "Failure parsing InsightCategory from value: '" <> e
            <> "'. Accepted values: fault"

instance ToText InsightCategory where
  toText = \case
    Fault -> "FAULT"

instance Hashable InsightCategory

instance NFData InsightCategory

instance ToByteString InsightCategory

instance ToQuery InsightCategory

instance ToHeader InsightCategory

instance FromJSON InsightCategory where
  parseJSON = parseJSONText "InsightCategory"
