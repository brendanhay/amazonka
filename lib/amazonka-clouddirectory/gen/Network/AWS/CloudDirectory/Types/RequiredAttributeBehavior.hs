{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior where

import Network.AWS.Prelude

data RequiredAttributeBehavior
  = NotRequired
  | RequiredAlways
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

instance FromText RequiredAttributeBehavior where
  parser =
    takeLowerText >>= \case
      "not_required" -> pure NotRequired
      "required_always" -> pure RequiredAlways
      e ->
        fromTextError $
          "Failure parsing RequiredAttributeBehavior from value: '" <> e
            <> "'. Accepted values: not_required, required_always"

instance ToText RequiredAttributeBehavior where
  toText = \case
    NotRequired -> "NOT_REQUIRED"
    RequiredAlways -> "REQUIRED_ALWAYS"

instance Hashable RequiredAttributeBehavior

instance NFData RequiredAttributeBehavior

instance ToByteString RequiredAttributeBehavior

instance ToQuery RequiredAttributeBehavior

instance ToHeader RequiredAttributeBehavior

instance ToJSON RequiredAttributeBehavior where
  toJSON = toJSONText

instance FromJSON RequiredAttributeBehavior where
  parseJSON = parseJSONText "RequiredAttributeBehavior"
