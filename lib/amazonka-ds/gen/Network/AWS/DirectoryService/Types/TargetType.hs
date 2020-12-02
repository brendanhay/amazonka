{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.TargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.TargetType where

import Network.AWS.Prelude

data TargetType = Account
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

instance FromText TargetType where
  parser =
    takeLowerText >>= \case
      "account" -> pure Account
      e ->
        fromTextError $
          "Failure parsing TargetType from value: '" <> e
            <> "'. Accepted values: account"

instance ToText TargetType where
  toText = \case
    Account -> "ACCOUNT"

instance Hashable TargetType

instance NFData TargetType

instance ToByteString TargetType

instance ToQuery TargetType

instance ToHeader TargetType

instance ToJSON TargetType where
  toJSON = toJSONText
