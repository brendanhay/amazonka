{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.UserFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.UserFilterType where

import Network.AWS.Prelude

data UserFilterType
  = ActivePending
  | All
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

instance FromText UserFilterType where
  parser =
    takeLowerText >>= \case
      "active_pending" -> pure ActivePending
      "all" -> pure All
      e ->
        fromTextError $
          "Failure parsing UserFilterType from value: '" <> e
            <> "'. Accepted values: active_pending, all"

instance ToText UserFilterType where
  toText = \case
    ActivePending -> "ACTIVE_PENDING"
    All -> "ALL"

instance Hashable UserFilterType

instance NFData UserFilterType

instance ToByteString UserFilterType

instance ToQuery UserFilterType

instance ToHeader UserFilterType

instance ToJSON UserFilterType where
  toJSON = toJSONText
