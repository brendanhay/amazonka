{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ChangeTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ChangeTypeEnum where

import Network.AWS.Prelude

data ChangeTypeEnum
  = A
  | D
  | M
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

instance FromText ChangeTypeEnum where
  parser =
    takeLowerText >>= \case
      "a" -> pure A
      "d" -> pure D
      "m" -> pure M
      e ->
        fromTextError $
          "Failure parsing ChangeTypeEnum from value: '" <> e
            <> "'. Accepted values: a, d, m"

instance ToText ChangeTypeEnum where
  toText = \case
    A -> "A"
    D -> "D"
    M -> "M"

instance Hashable ChangeTypeEnum

instance NFData ChangeTypeEnum

instance ToByteString ChangeTypeEnum

instance ToQuery ChangeTypeEnum

instance ToHeader ChangeTypeEnum

instance FromJSON ChangeTypeEnum where
  parseJSON = parseJSONText "ChangeTypeEnum"
