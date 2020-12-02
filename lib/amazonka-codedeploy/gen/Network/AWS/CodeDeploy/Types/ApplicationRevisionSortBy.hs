{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ApplicationRevisionSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ApplicationRevisionSortBy where

import Network.AWS.Prelude

data ApplicationRevisionSortBy
  = FirstUsedTime
  | LastUsedTime
  | RegisterTime
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

instance FromText ApplicationRevisionSortBy where
  parser =
    takeLowerText >>= \case
      "firstusedtime" -> pure FirstUsedTime
      "lastusedtime" -> pure LastUsedTime
      "registertime" -> pure RegisterTime
      e ->
        fromTextError $
          "Failure parsing ApplicationRevisionSortBy from value: '" <> e
            <> "'. Accepted values: firstusedtime, lastusedtime, registertime"

instance ToText ApplicationRevisionSortBy where
  toText = \case
    FirstUsedTime -> "firstUsedTime"
    LastUsedTime -> "lastUsedTime"
    RegisterTime -> "registerTime"

instance Hashable ApplicationRevisionSortBy

instance NFData ApplicationRevisionSortBy

instance ToByteString ApplicationRevisionSortBy

instance ToQuery ApplicationRevisionSortBy

instance ToHeader ApplicationRevisionSortBy

instance ToJSON ApplicationRevisionSortBy where
  toJSON = toJSONText
