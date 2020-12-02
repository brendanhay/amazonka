{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateBehavior where

import Network.AWS.Prelude

data UpdateBehavior
  = UBLog
  | UBUpdateInDatabase
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

instance FromText UpdateBehavior where
  parser =
    takeLowerText >>= \case
      "log" -> pure UBLog
      "update_in_database" -> pure UBUpdateInDatabase
      e ->
        fromTextError $
          "Failure parsing UpdateBehavior from value: '" <> e
            <> "'. Accepted values: log, update_in_database"

instance ToText UpdateBehavior where
  toText = \case
    UBLog -> "LOG"
    UBUpdateInDatabase -> "UPDATE_IN_DATABASE"

instance Hashable UpdateBehavior

instance NFData UpdateBehavior

instance ToByteString UpdateBehavior

instance ToQuery UpdateBehavior

instance ToHeader UpdateBehavior

instance ToJSON UpdateBehavior where
  toJSON = toJSONText

instance FromJSON UpdateBehavior where
  parseJSON = parseJSONText "UpdateBehavior"
