{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ApprovalModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ApprovalModel where

import Network.AWS.Prelude

data ApprovalModel
  = Automatic
  | Manual
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

instance FromText ApprovalModel where
  parser =
    takeLowerText >>= \case
      "automatic" -> pure Automatic
      "manual" -> pure Manual
      e ->
        fromTextError $
          "Failure parsing ApprovalModel from value: '" <> e
            <> "'. Accepted values: automatic, manual"

instance ToText ApprovalModel where
  toText = \case
    Automatic -> "AUTOMATIC"
    Manual -> "MANUAL"

instance Hashable ApprovalModel

instance NFData ApprovalModel

instance ToByteString ApprovalModel

instance ToQuery ApprovalModel

instance ToHeader ApprovalModel

instance ToJSON ApprovalModel where
  toJSON = toJSONText

instance FromJSON ApprovalModel where
  parseJSON = parseJSONText "ApprovalModel"
