{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.PropertyKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.PropertyKey where

import Network.AWS.Prelude

data PropertyKey
  = LaunchRole
  | Owner
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

instance FromText PropertyKey where
  parser =
    takeLowerText >>= \case
      "launch_role" -> pure LaunchRole
      "owner" -> pure Owner
      e ->
        fromTextError $
          "Failure parsing PropertyKey from value: '" <> e
            <> "'. Accepted values: launch_role, owner"

instance ToText PropertyKey where
  toText = \case
    LaunchRole -> "LAUNCH_ROLE"
    Owner -> "OWNER"

instance Hashable PropertyKey

instance NFData PropertyKey

instance ToByteString PropertyKey

instance ToQuery PropertyKey

instance ToHeader PropertyKey

instance ToJSON PropertyKey where
  toJSON = toJSONText

instance FromJSON PropertyKey where
  parseJSON = parseJSONText "PropertyKey"
