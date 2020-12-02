{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.RelativeFileVersionEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.RelativeFileVersionEnum where

import Network.AWS.Prelude

data RelativeFileVersionEnum
  = After
  | Before
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

instance FromText RelativeFileVersionEnum where
  parser =
    takeLowerText >>= \case
      "after" -> pure After
      "before" -> pure Before
      e ->
        fromTextError $
          "Failure parsing RelativeFileVersionEnum from value: '" <> e
            <> "'. Accepted values: after, before"

instance ToText RelativeFileVersionEnum where
  toText = \case
    After -> "AFTER"
    Before -> "BEFORE"

instance Hashable RelativeFileVersionEnum

instance NFData RelativeFileVersionEnum

instance ToByteString RelativeFileVersionEnum

instance ToQuery RelativeFileVersionEnum

instance ToHeader RelativeFileVersionEnum

instance ToJSON RelativeFileVersionEnum where
  toJSON = toJSONText

instance FromJSON RelativeFileVersionEnum where
  parseJSON = parseJSONText "RelativeFileVersionEnum"
