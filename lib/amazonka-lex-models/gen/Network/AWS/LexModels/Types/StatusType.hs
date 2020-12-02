{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.StatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.StatusType where

import Network.AWS.Prelude

data StatusType
  = Detected
  | Missed
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

instance FromText StatusType where
  parser =
    takeLowerText >>= \case
      "detected" -> pure Detected
      "missed" -> pure Missed
      e ->
        fromTextError $
          "Failure parsing StatusType from value: '" <> e
            <> "'. Accepted values: detected, missed"

instance ToText StatusType where
  toText = \case
    Detected -> "Detected"
    Missed -> "Missed"

instance Hashable StatusType

instance NFData StatusType

instance ToByteString StatusType

instance ToQuery StatusType

instance ToHeader StatusType

instance ToJSON StatusType where
  toJSON = toJSONText
