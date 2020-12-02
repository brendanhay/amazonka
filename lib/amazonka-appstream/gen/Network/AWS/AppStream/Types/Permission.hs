{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Permission where

import Network.AWS.Prelude

data Permission
  = Disabled
  | Enabled
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

instance FromText Permission where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "enabled" -> pure Enabled
      e ->
        fromTextError $
          "Failure parsing Permission from value: '" <> e
            <> "'. Accepted values: disabled, enabled"

instance ToText Permission where
  toText = \case
    Disabled -> "DISABLED"
    Enabled -> "ENABLED"

instance Hashable Permission

instance NFData Permission

instance ToByteString Permission

instance ToQuery Permission

instance ToHeader Permission

instance ToJSON Permission where
  toJSON = toJSONText

instance FromJSON Permission where
  parseJSON = parseJSONText "Permission"
