{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.UpdateDataRetentionOperation where

import Network.AWS.Prelude

data UpdateDataRetentionOperation
  = DecreaseDataRetention
  | IncreaseDataRetention
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

instance FromText UpdateDataRetentionOperation where
  parser =
    takeLowerText >>= \case
      "decrease_data_retention" -> pure DecreaseDataRetention
      "increase_data_retention" -> pure IncreaseDataRetention
      e ->
        fromTextError $
          "Failure parsing UpdateDataRetentionOperation from value: '" <> e
            <> "'. Accepted values: decrease_data_retention, increase_data_retention"

instance ToText UpdateDataRetentionOperation where
  toText = \case
    DecreaseDataRetention -> "DECREASE_DATA_RETENTION"
    IncreaseDataRetention -> "INCREASE_DATA_RETENTION"

instance Hashable UpdateDataRetentionOperation

instance NFData UpdateDataRetentionOperation

instance ToByteString UpdateDataRetentionOperation

instance ToQuery UpdateDataRetentionOperation

instance ToHeader UpdateDataRetentionOperation

instance ToJSON UpdateDataRetentionOperation where
  toJSON = toJSONText
