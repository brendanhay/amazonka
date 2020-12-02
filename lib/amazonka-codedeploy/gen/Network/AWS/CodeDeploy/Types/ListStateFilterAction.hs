{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.ListStateFilterAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.ListStateFilterAction where

import Network.AWS.Prelude

data ListStateFilterAction
  = Exclude
  | Ignore
  | Include
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

instance FromText ListStateFilterAction where
  parser =
    takeLowerText >>= \case
      "exclude" -> pure Exclude
      "ignore" -> pure Ignore
      "include" -> pure Include
      e ->
        fromTextError $
          "Failure parsing ListStateFilterAction from value: '" <> e
            <> "'. Accepted values: exclude, ignore, include"

instance ToText ListStateFilterAction where
  toText = \case
    Exclude -> "exclude"
    Ignore -> "ignore"
    Include -> "include"

instance Hashable ListStateFilterAction

instance NFData ListStateFilterAction

instance ToByteString ListStateFilterAction

instance ToQuery ListStateFilterAction

instance ToHeader ListStateFilterAction

instance ToJSON ListStateFilterAction where
  toJSON = toJSONText
