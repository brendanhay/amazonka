{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchAction where

import Network.AWS.Prelude

data PatchAction
  = AllowAsDependency
  | Block
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

instance FromText PatchAction where
  parser =
    takeLowerText >>= \case
      "allow_as_dependency" -> pure AllowAsDependency
      "block" -> pure Block
      e ->
        fromTextError $
          "Failure parsing PatchAction from value: '" <> e
            <> "'. Accepted values: allow_as_dependency, block"

instance ToText PatchAction where
  toText = \case
    AllowAsDependency -> "ALLOW_AS_DEPENDENCY"
    Block -> "BLOCK"

instance Hashable PatchAction

instance NFData PatchAction

instance ToByteString PatchAction

instance ToQuery PatchAction

instance ToHeader PatchAction

instance ToJSON PatchAction where
  toJSON = toJSONText

instance FromJSON PatchAction where
  parseJSON = parseJSONText "PatchAction"
