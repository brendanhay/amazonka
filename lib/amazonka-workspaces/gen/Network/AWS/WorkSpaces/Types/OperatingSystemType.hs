{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.OperatingSystemType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.OperatingSystemType where

import Network.AWS.Prelude

data OperatingSystemType
  = Linux
  | Windows
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

instance FromText OperatingSystemType where
  parser =
    takeLowerText >>= \case
      "linux" -> pure Linux
      "windows" -> pure Windows
      e ->
        fromTextError $
          "Failure parsing OperatingSystemType from value: '" <> e
            <> "'. Accepted values: linux, windows"

instance ToText OperatingSystemType where
  toText = \case
    Linux -> "LINUX"
    Windows -> "WINDOWS"

instance Hashable OperatingSystemType

instance NFData OperatingSystemType

instance ToByteString OperatingSystemType

instance ToQuery OperatingSystemType

instance ToHeader OperatingSystemType

instance FromJSON OperatingSystemType where
  parseJSON = parseJSONText "OperatingSystemType"
