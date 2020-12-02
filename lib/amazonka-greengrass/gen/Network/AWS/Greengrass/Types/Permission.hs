{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.Permission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Permission where

import Network.AWS.Prelude

-- | The type of permission a function has to access a resource.
data Permission
  = RO
  | RW
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
      "ro" -> pure RO
      "rw" -> pure RW
      e ->
        fromTextError $
          "Failure parsing Permission from value: '" <> e
            <> "'. Accepted values: ro, rw"

instance ToText Permission where
  toText = \case
    RO -> "ro"
    RW -> "rw"

instance Hashable Permission

instance NFData Permission

instance ToByteString Permission

instance ToQuery Permission

instance ToHeader Permission

instance ToJSON Permission where
  toJSON = toJSONText

instance FromJSON Permission where
  parseJSON = parseJSONText "Permission"
