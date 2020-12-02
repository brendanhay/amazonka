{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.AddOnType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.AddOnType where

import Network.AWS.Prelude

data AddOnType = AutoSnapshot
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

instance FromText AddOnType where
  parser =
    takeLowerText >>= \case
      "autosnapshot" -> pure AutoSnapshot
      e ->
        fromTextError $
          "Failure parsing AddOnType from value: '" <> e
            <> "'. Accepted values: autosnapshot"

instance ToText AddOnType where
  toText = \case
    AutoSnapshot -> "AutoSnapshot"

instance Hashable AddOnType

instance NFData AddOnType

instance ToByteString AddOnType

instance ToQuery AddOnType

instance ToHeader AddOnType

instance ToJSON AddOnType where
  toJSON = toJSONText
