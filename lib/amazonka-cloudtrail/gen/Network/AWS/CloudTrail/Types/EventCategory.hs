{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.EventCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.EventCategory where

import Network.AWS.Prelude

data EventCategory = Insight
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

instance FromText EventCategory where
  parser =
    takeLowerText >>= \case
      "insight" -> pure Insight
      e ->
        fromTextError $
          "Failure parsing EventCategory from value: '" <> e
            <> "'. Accepted values: insight"

instance ToText EventCategory where
  toText = \case
    Insight -> "insight"

instance Hashable EventCategory

instance NFData EventCategory

instance ToByteString EventCategory

instance ToQuery EventCategory

instance ToHeader EventCategory

instance ToJSON EventCategory where
  toJSON = toJSONText
